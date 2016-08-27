{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-} -- not sure which of these I actually need
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- what I want to do:
-- send over to js (map to web audio functionality)

-- first step: connect to js

import Control.Concurrent.STM
import Control.Natural
import qualified Control.Remote.Applicative as APP
import Control.Remote.Monad
import Control.Remote.Monad.Packet.Weak as WP
import Control.Remote.Monad.Packet.Strong as SP
import Control.Remote.Monad.Packet.Applicative as AP
import Control.Remote.Monad.Packet.Alternative as Alt

import Data.Aeson (FromJSON(..),Value(..),withText)
import Data.Aeson.Types (Parser,parse,Result(..))
import Data.Char
import Data.Default.Class
import Data.Monoid ((<>))
import qualified Data.Text as T

import Debug.Trace

import Network.Wai.Middleware.Static

import System.IO.Unsafe (unsafePerformIO)

import Text.Read
import qualified Text.Read.Lex as L

import qualified Web.Scotty.Comet as KC
import Web.Scotty

-- WORKING EXAMPLE
-- main :: IO ()
-- main = do
--   webAudio 3000 $ \doc -> do
--     send doc $ do
--       _ <- createOscillator 400 0 Sine
--       _ <- createOscillator 404 4 Sine
--       return ()

main :: IO ()
main = do
  webAudio 3000 $ \doc -> do
    send doc $ do
      _ <- createOscillator 400 0 Sine
      _ <- createOscillator 404 4 Sine
      return ()
    -- sendApp doc $ WebAudio $ do
    --   createOscillator 400 0 Sine
    --   return ()
  -- kcomet <- KC.kCometPlugin

  
data Command :: * where
  -- CreateOscillator :: OscillatorNode -> Command

data Procedure :: * -> * where
  CreateOscillator :: Double -> Double -> OscillatorNodeType -> Procedure OscillatorNode

-- | And AudioNode is an interface for any audio processing module in the Web Audio API
class AudioNode a where
  jsAudioNode           :: a -> T.Text
  numberOfInputs        :: a -> Int
  numberOfOutputs       :: a -> Int
  channelCount          :: a -> Int -- potentially change to maybe
  channelCountMode      :: a -> ChannelCountMode
  channelInterpretation :: a -> ChannelInterpretation 

-- | Instantizes OscillatorNode with the default values
instance AudioNode OscillatorNode where
  jsAudioNode           = showJS
  numberOfInputs        = numberOfInputsOsc
  numberOfOutputs       = numberOfOutputsOsc
  channelCount          = channelCountOsc
  channelCountMode      = channelCountModeOsc
  channelInterpretation = channelInterpretationOsc
    
data ChannelCountMode = Max | ClampedMax | Explicit
  deriving (Eq)

instance Show ChannelCountMode where
  show Max        = "max"
  show ClampedMax = "clamped-max"
  show Explicit   = "explicit"

instance Read ChannelCountMode where
  readPrec =
    parens
    ( do L.Ident s <- lexP
         case s of
           "max"         -> return Max
           "clamped-max" -> return ClampedMax
           "explicit"    -> return Explicit
           _             -> pfail
    )
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance FromJSON ChannelCountMode where
  parseJSON = withText "ChannelCountMode" $ \s ->
    case s of
      "max"         -> return Max
      "clamped-max" -> return ClampedMax
      "explicit"    -> return Explicit
      _             -> fail "Parsing ChannelCountMode value failed: expected \"max\", \"clamped-max\", or \"Explicit\""
  
class JSArg a where
      -- | Display a value as JavaScript data.
      showJS :: a -> T.Text

instance JSArg OscillatorNode where
  showJS = jsOscillatorNode

jsOscillatorNode :: OscillatorNode -> T.Text
jsOscillatorNode (OscillatorNode n _ _ _ _ _ _ _ _) = "audios[" <> tshow n <> "]"

data ChannelInterpretation = Speakers | Discrete
  deriving (Eq)

instance Read ChannelInterpretation where
  readPrec =
    parens
    ( do L.Ident s <- lexP
         case s of
           "speakers" -> return Speakers
           "discrete" -> return Discrete
           _          -> pfail
    )
  readListPrec = readListPrecDefault
  readList     = readListDefault
  
instance Show ChannelInterpretation where
  show Speakers = "speakers"
  show Discrete = "discrete"

instance FromJSON ChannelInterpretation where
  parseJSON = withText "ChannelInterpretation" $ \s ->
    case s of
      "speakers" -> return Speakers
      "discrete" -> return Discrete
      _          -> fail "Parsing ChannelInterpretation value failed: expected \"speakers\", or \"discrete\""  

-- | creates an oscillator with a frequency (in hertz), a detuning value (in cents), and an OscillatorNodeType (e.g. a sine wave, square wave, etc.)
createOscillator :: Double -> Double -> OscillatorNodeType -> WebAudio OscillatorNode 
-- createOscillator :: Double -> Double -> OscillatorNodeType -> RemoteMonad Command Procedure OscillatorNode
createOscillator freq det osctype = WebAudio $ procedure (CreateOscillator freq det osctype)

newtype WebAudio a = WebAudio (RemoteMonad Command Procedure a)
  deriving (Functor, Applicative, Monad)

send :: KC.Document -> WebAudio a -> IO a
send = sendApp

sendApp :: KC.Document -> WebAudio a -> IO a
sendApp d (WebAudio m) = (run $ runMonad $ nat (runAP d)) m

-- runAP :: KC.Document -> WebAudio a -> IO a
runAP :: KC.Document -> ApplicativePacket Command Procedure a -> IO a
runAP d pkt =
  case AP.superCommand pkt of
    Just a -> do -- is a Command
      -- let cmds = formatPackets pkt ""
      let cmds = "" -- temp solution
      KC.send d cmds
      return a
    Nothing -> case pkt of
      AP.Command cmd -> undefined -- do KC.send d (formatCommand cmd)
      AP.Procedure p -> sendProcedure d p ""

sendProcedure :: KC.Document -> Procedure a -> T.Text -> IO a
sendProcedure d p@(CreateOscillator freq det nodetype) _ = do
  uq <- atomically getUniq
  KC.send d $ "CreateOscillator(" <> tshow freq <> "," <> tshow det <> ",'" <>
    tshow nodetype <> "')(" <> tshow uq <> ");"
  v <- KC.getReply d uq
  case parse (parseProcedure p) v of
    Error msg -> fail msg
    Success a -> return a
  
parseProcedure :: Procedure a -> Value -> Parser a
parseProcedure (CreateOscillator {}) o = uncurry9 OscillatorNode <$> parseJSON o

-- yeah it's gross
uncurry9 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b) ->
            (a1, a2, a3, a4, a5, a6, a7, a8, a9) -> b
uncurry9 f (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f a1 a2 a3 a4 a5 a6 a7 a8 a9

  
-- runWA 
-- sendWP :: KC.Document -> WeakPacket Command Procedure a -> IO a
-- sendWP doc (WP.Command (CreateOscillator o)) = run $ runMonad $ nat (do
--   KC.send doc "var osc1 = audioCtx.createOscillator();")
  
-- sendWP _ (WP.Procedure Dummy ) = do
--   return 42
  -- KC.send doc " "
  
-- sendWeak :: RemoteMonad Command Procedure a -> IO a
-- sendWeak = run $ runMonad $ nat (\pkt -> do putStrLn "-----"; runWP pkt)
-- sendWeak = run $ runMonad $ nat

-- sendWeak KC.Document -> [Text] -> IO ()
-- sendWeak docs cmds = KC.send docs (unlines cmds)


webAudio :: WAOptions -> (KC.Document -> IO ()) -> IO ()
webAudio opts actions = do
  kcomet <- KC.kCometPlugin

  let pol = only [ ("","index.html")
                 , ("js/kansas-comet.js",kcomet)
                 ]
        <|> (hasPrefix "js/") >-> addBase "."

  let kcopts = KC.Options {KC.prefix = "/example", KC.verbose = if debug opts then 3 else 0}
  
  connectApp <- KC.connect kcopts $ \kc_doc -> do
    actions kc_doc
    -- sendApp kc_doc $ WebAudio $ actions
      -- osc <- createOscillator 400 0 Sine
      -- traceShow osc $ return ()
      -- return ()
      

  scotty (port opts) $ do
    middleware $ staticPolicy pol
    connectApp
    
  return()
  
data WAOptions = WAOptions
  { port          :: Int,
    events        :: [Int], -- not implemented yet, blank canvas uses [EventName],
    debug         :: Bool,
    root          :: String, -- location of static files
    -- middleware :: [KC.Middleware],
    weak          :: Bool
  }

-- webaudio options
instance Num WAOptions where
  (+)           = error "No arith. in WAOptions"
  (-)           = error "No arith. in WAOptions"
  (*)           = error "No arith. in WAOptions"
  abs           = error "No arith. in WAOptions"  
  signum        = error "No arith. in WAOptions"
  fromInteger n = WAOptions { port = fromInteger n ,
                            events = [] ,
                            debug = False,
                            root = "." ,
                            weak = False
                          }

oscType :: KC.Document -> OscillatorNodeType -> IO ()
oscType doc t = do
  KC.send doc (T.concat ["osc.type = '" , T.pack . show $ t , "';"])
  -- KC.send doc "osc.type = 'square';"
  
data OscillatorNodeType = Sine | Square | Sawtooth | Triangle | Custom
     deriving (Eq)

instance Read OscillatorNodeType where
  readPrec =
    parens
    ( do L.Ident s <- lexP
         case s of
           "sine"     -> return Sine
           "square"   -> return Square
           "sawtooth" -> return Sawtooth
           "triangle" -> return Triangle
           "custom"   -> return Custom
           _          -> pfail
    )
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Show OscillatorNodeType where
  show Sine     = "sine"
  show Square   = "square"
  show Sawtooth = "sawtooth"
  show Triangle = "triangle"
  show Custom   = "custom"

instance FromJSON OscillatorNodeType where
  parseJSON = withText "OscillatorNodeType" $ \s ->
    case s of
      "sine"     -> return Sine
      "square"   -> return Square
      "sawtooth" -> return Sawtooth
      "triangle" -> return Triangle
      "custom"   -> return Custom      
      _ -> fail "Parsing OscillatorNodeType value failed: expected \"sine\", \"square\", \"sawtooth\", \"Triangel\", or \"custom\""

-- | OscillatorNode represents a periodic waveform with a frequency (in hertz), detuning (in cents), an OscillatorNodeType (e.g. a sine wave, square wave, etc.), etc.

data OscillatorNode = OscillatorNode {
  index                    :: !Int,
  frequency                :: !Double,
  detune                   :: !Double,
  typeOsc                  :: !OscillatorNodeType,
  numberOfInputsOsc        :: !Int,
  numberOfOutputsOsc       :: !Int,
  channelCountOsc          :: !Int,
  channelCountModeOsc      :: !ChannelCountMode,
  channelInterpretationOsc :: !ChannelInterpretation
}
  deriving (Read,Show,Eq)                      
  
tshow :: Show a => a -> T.Text
tshow a = T.pack $ show a

{-# NOINLINE uniqVar #-}
uniqVar :: TVar Int
uniqVar = unsafePerformIO $ newTVarIO 0

getUniq :: STM Int
getUniq = do
      u <- readTVar uniqVar
      writeTVar uniqVar (u + 1)
      return u
