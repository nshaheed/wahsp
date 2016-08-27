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
import Control.Remote.Applicative as APP
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

import Network.Wai.Middleware.Static

import System.IO.Unsafe (unsafePerformIO)

import Text.Read
import qualified Text.Read.Lex as L

import qualified Web.Scotty.Comet as KC
import Web.Scotty

data Command :: * where
  -- CreateOscillator :: OscillatorNode -> Command

data Procedure :: * -> * where
  Dummy :: Procedure Int
  CreateOscillator :: Double -> Double -> OscillatorNodeType -> Procedure OscillatorNode
  
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

--  showi (NewImage url') = "NewImage(" <> jsText url' <> singleton ')'
-- (CreateOscillator ..) = "
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
-- createOscillator :: OscillatorNode -> RemoteMonad Command Procedure ()
-- createOscillator o = command (CreateOscillator o)

newtype WebAudio a = WebAudio (RemoteMonad Command Procedure a)
  deriving (Functor, Applicative, Monad)

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

--  KC.send (theComet cxt) . toStrict . toLazyText $ surround "syncToFrame(function(){" "});" <> cmds
--  send' $ cmds <> showi query <> singleton '(' <> showi uq <> singleton ',' <> jsCanvasContext c <> ");"

sendProcedure :: KC.Document -> Procedure a -> T.Text -> IO a
sendProcedure d p@(CreateOscillator freq det nodetype) _ = do
  uq <- atomically getUniq
  KC.send d $ "CreateOscillator(" <> tshow freq <> "," <> tshow det <> "," <> tshow nodetype <> ")(" <>
    tshow uq <> "," <> ");"
  v <- KC.getReply d uq
  case parse (parseProcedure p) v of
    Error msg -> fail msg
    Success a -> return a
  
parseProcedure :: Procedure a -> Value -> Parser a
parseProcedure (CreateOscillator {}) o = uncurry9 OscillatorNode <$> parseJSON o

uncurry9 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b) ->
            (a1, a2, a3, a4, a5, a6, a7, a8, a9) -> b
uncurry9 f (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f a1 a2 a3 a4 a5 a6 a7 a8 a9
--   where
--     formatPackets :: ApplicativePacket Command Procedure a -> T.Text -> T.Text
--     formatPackets pakt cmds = T.unlines[formatPacket pakt,cmds]
      
--     formatPacket :: ApplicativePacket Command Procedure a -> T.Text
--     formatPacket pakt =
--       case pakt of
--         AP.Command cmd -> formatCommand cmd
--         AP.Procedure (Dummy) -> " "

--     formatCommand :: Command -> T.Text
--     formatCommand (CreateOscillator (OscillatorNode freq det wave)) =
--       T.unlines [T.concat ["osc.frequency.value = ", T.pack . show $ freq, ";"],
--                  T.concat ["osc.detune.value = ",T.pack . show $ det,";"],
--                  T.concat ["osc.type = '" , T.pack . show $ wave , "';"]
--                 ]
-- runWA d (RemoteMonad Command Procedure a ) =
  
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

main :: IO ()
main = do
  kcomet <- KC.kCometPlugin

  let pol = only [ ("","index.html")
                 , ("js/kansas-comet.js",kcomet)
                 ]
        <|> (hasPrefix "js/") >-> addBase "."

  connectApp <- KC.connect opts $ \kc_doc -> do
    sendApp kc_doc $ do
      -- APP.procedure $ CreateOscillator 400 0 Sine
      CreateOscillator 400 0 Sine      
      

  scotty 3000 $ do
    middleware $ staticPolicy pol
    connectApp
    
  return()

opts :: KC.Options
opts = def { KC.prefix = "/example"}
-- opts = KC.Options { prefix = "/example"}

test :: KC.Document -> IO ()
test doc = do
  putStrLn "in test"
  -- KC.send doc "testOsc()"
  -- oscType doc Sawtooth
  -- KC.send doc "osc.start();" -- both work
  -- sendApp doc (WebAudio (createOscillator (oscillatorNode 220 0 Square)))
  -- KC.send doc "osc.start();" -- both work
  -- KC.send doc "console.log(\"test\");"
  -- KC.send doc "var osc1 = audioCtx.createOscillator();"
  -- KC.send doc (T.unlines ["osc1.frequency.value = 350;",
  --                        "console.log(osc1.frequency.value);",
  --                        "osc1.start();"
  --                        ])
  -- KC.send doc "testE();"
  -- KC.send doc "osc.start();"
  -- KC.send doc "console.log(osc1.frequency.value;"
  return ()

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

-- | OscillatorNode with the frequency, detuning, and OscillatorNodeType
-- data OscillatorNode = OscillatorNode !Int !Double !Double !OscillatorNodeType
--      deriving (Read, Show, Eq)

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
  
-- oscillatorNode :: Double -> Double -> OscillatorNodeType -> OscillatorNode
-- oscillatorNode = OscillatorNode

-- start :: OscillatorNode -> Double -> IO ()

-- sendToCanvas :: DeviceContext -> Instr -> IO ()
-- sendToCanvas cxt cmds = do
--       KC.send (theComet cxt) . toStrict . toLazyText $ surround "syncToFrame(function(){"  "});" <> cmds

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
