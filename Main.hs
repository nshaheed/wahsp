{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-} -- not sure which of these I actually need
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- to add a command:
-- add to Command data type
-- create a function -> WebAudio ()
-- add pattern to formatCommand
-- add pattern to sendProcedure

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
-- import Data.Monoid ((<>))
import Data.Semigroup hiding (Max(..))
-- import Data.Semigroup
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
      osc1 <- createOscillator 200 0 Sine
      osc2 <- createOscillator 404 4 Sine
      gain' <- createGain 0.5

      -- let t = Node (AudNode osc1) (EndNode (AudNode osc2))
      -- let t' = Node (AudNode osc1) (EndNode (AudNode gain'))

      -- let t'' = connector osc1 (connector osc2 (eNode osc1))
      -- -- let t''' = (osc1 .|. (osc2 .|. (Node (AudNode gain' (eNode gain'))))) -- (eNode gain')
      -- let t'''' = (osc1 .|. (osc2 .|. (gain' .||. (eNode gain'))))
      -- let tt = osc1 .||. (eNode gain')
      -- let tt' = osc1 .||. eNode gain'
      -- let tt'' = osc1 .|. osc2 .||. eNode gain'

      -- connecting an oscillator to another oscillator doesn't work, not enough inlets
      -- let g = osc1 .|. osc2 .||. eNode osc2
      let g = osc1 .|. gain' .||. eCtx

      connect g
      -- connect g

      -- let g' = osc1 .|. gain' .||. eNode osc2
      -- let result = audioGraphConnect g'
      
      -- connects [Left osc1, Right gain] (Just AudioContext )
      -- connect osc1 osc2
      -- connect :: AudioGraph [AudioNode] b
      -- (<|>) :: [AudioNode] -> b -> AudioGraph b
      -- connect :: Connector = AudioNode Connector | AudioNode | AudioParam | AudioContext
      -- connect osc1 <> gain <|> audioContext
      -- conn
      start osc1
      return ()
    -- sendApp doc $ WebAudio $ do
    --   createOscillator 400 0 Sine
    --   return ()
  -- kcomet <- KC.kCometPlugin
    -- KC.send doc $ "sounds[0].start();"

data Command :: * where
  Start     :: OscillatorNode -> Command
  StartWhen :: OscillatorNode -> Double -> Command
  Stop      :: OscillatorNode -> Command
  StopWhen  :: OscillatorNode -> Double -> Command
  Connect   :: AudioGraph AudNode b -> Command
  -- Connect   :: AudioNode a => a -> a -> Command
  -- Connects :: AudioGraph a b -> Command 

-- data AudioGraph :: * -> * -> * where
--   AudioGraph :: (AudioNode a, AudioParam b) => [Either a b] -> Maybe AudioContext ->
--                    AudioGraph a b

data AudioGraph :: * -> * -> * where
  Node     :: AudNode -> AudioGraph AudNode b -> AudioGraph AudNode b
  EndNode  :: AudNode -> AudioGraph AudNode AudNode
  EndParam :: AudioParam -> AudioGraph AudNode AudioParam
  EndCtx   :: AudioContext -> AudioGraph AudNode AudioContext

data AudNode where
  AudNode :: (AudioNode a) => a -> AudNode


-- connector :: (AudioNode a) => a -> (a -> AudioGraph AudNode b) -> AudioGraph AudNode b
-- connector node graph = Node (AudNode node) (\g -> graph g)

connector node = Node (AudNode node)

(.|.) a b = connector a b
infix 7 .|.

(.||.) a b = Node (AudNode a) b
infix 8 .||.

eNode a = EndNode (AudNode a)
eParam = EndParam
eCtx = EndCtx AudioContext

-- data (AudioNode a) => AudioGraph a =
-- data AudioNode a => AudioGraph a = 
--   Node a (AudioGraph a) | EndNode a | EndParam AudioParam | EndCtx AudioContext
--   deriving (Read, Show)

-- data O where
--   N :: AudioNode a => a -> O
--   P :: AudioParam -> O
--   C :: AudioContext -> O

-- data T where
--   I :: (Show a) => a -> T
--   E :: (Eq b) => b -> T
-- connector :: AudioGraph a b -> b -> AudioGraph a b
-- connector 

-- connector' :: AudioNode a => a -> AudioGraph a b -> AudioGraph a b
-- connector' = Node

-- connector' :: AudioNode a => a -> O -> AudioGraph a -- something like this

-- instance Semigroup (AudioGraph a b) where
--   a <> 
--   a <> b = b
  
  -- AudioGraph :: (AudioNode a) => 

-- connects :: [AudioGraph] -> T.Text
-- connects (c@(AudioCtx _):_) = showAudioGraph c
-- connects (x:[]) = showAudioGraph x
-- connects (x:xs) = showAudioGraph x <> ".connect(" <> connects xs <> ")"

-- showAudioGraph :: AudioGraph -> T.Text
-- showAudioGraph (AudioCtx AudioContext) = "audioCtx.destination"
-- showAudioGraph (AudioParam a) = showtJS a
-- showAudioGraph (AudioNode a)  = showtJS a

-- testConnects :: T.Text
-- testConnects =
  
-- data AudioGraph where
--   AudioNode  :: AudioNode a => a -> AudioGraph
--   AudioParam :: AudioParam a => a -> AudioGraph
--   AudioCtx   :: AudioContext -> AudioGraph

-- audioGraph ((Left a):xs) _ = ""
-- audioGraph ((Right b):xs) _ = ""
-- audioGraph [] (Just a) = ""
-- audoGraph [] (Nothing) = ""

-- connectToContext :: [Either AudioNode AudioParam] -> WebAudio ()
-- connectToContext

-- class AudioGraphInit = AudioGraphInit a | b
-- connect :: AudioNoda a => a -> a -> Command
-- connects :: AudioNode a => [a] -> Command

-- class JSArg a => AudioGraph a where
--   connecttmp :: AudioGraph b => a -> b -> T.Text

-- instance AudioGraph (AudioNode a) where
--   connecttmp a = 
  
data Procedure :: * -> * where
  CreateOscillator :: Double -> Double -> OscillatorNodeType -> Procedure OscillatorNode
  CreateGain :: Double -> Procedure GainNode

-- | And AudioNode is an interface for any audio processing module in the Web Audio API
class JSArg a => AudioNode a where
  -- jsAudioNode           :: a -> T.Text
  numberOfInputs        :: a -> Int
  numberOfOutputs       :: a -> Int
  channelCount          :: a -> Int -- potentially change to maybe
  channelCountMode      :: a -> ChannelCountMode
  channelInterpretation :: a -> ChannelInterpretation 

-- | Instantizes OscillatorNode with the default values
instance AudioNode OscillatorNode where
  -- jsAudioNode           = showtJS
  numberOfInputs        = numberOfInputsOsc
  numberOfOutputs       = numberOfOutputsOsc
  channelCount          = channelCountOsc
  channelCountMode      = channelCountModeOsc
  channelInterpretation = channelInterpretationOsc

-- class JSArg a => AudioParam a where
--   defaultValue :: a -> Double
--   maxValue     :: a -> Double
--   minValue     :: a -> Double
--   value        :: a -> Double

-- instance AudioParam Gain where
--   defaultValue = defaultValueGain
--   maxValue     = maxValueGain
--   minValue     = minValueGain
--   value        = valueGain

-- type AudioParam = Double

-- data AudioParam = AudioParam {
--   name         :: !T.Text,
--   defaultValue :: !Double,
--   maxValue     :: !Double,
--   minValue     :: !Double,
--   value        :: !Double
-- }

instance AudioNode GainNode where
  numberOfInputs        = numberOfInputsGain
  numberOfOutputs       = numberOfOutputsGain
  channelCount          = channelCountGain
  channelCountMode      = channelCountModeGain
  channelInterpretation = channelInterpretationGain  

data AudioParam = AudioParam AudioParamType Int Double 
  deriving (Read,Show)

data GainNode = GainNode {
  indexGain                 :: !Int,
  gain                      :: !AudioParam,
  numberOfInputsGain        :: !Int,
  numberOfOutputsGain       :: !Int,
  channelCountGain          :: !Int,
  channelCountModeGain      :: !ChannelCountMode,
  channelInterpretationGain :: !ChannelInterpretation  
}
  deriving (Show, Read)

-- the audio context, this is pre-existing in the js, and only one is needed
data AudioContext = AudioContext
  deriving (Eq, Read)

audioContext = AudioContext

instance Show AudioContext where
  show AudioContext = "audioCtx"

data ChannelCountMode = Max | ClampedMax | Explicit
  deriving (Eq)

data AudioParamType = Gain
  deriving (Eq,Read)

instance Show AudioParamType where
  show Gain = "gain"
  
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
      showtJS :: a -> T.Text

instance JSArg OscillatorNode where
  showtJS = jsOscillatorNode

instance JSArg AudioContext where
  showtJS a = tshow a

instance JSArg AudioParam where
  showtJS (AudioParam ptype idx _) = "sounds[" <> tshow idx <> "]." <> tshow ptype

-- instance JSArg AudioParam where
--   showtJS _ = "gain"

instance (JSArg a, JSArg b) => JSArg (Either a b) where
  showtJS (Left a)  = showtJS a
  showtJS (Right b) = showtJS b

instance JSArg GainNode where
  showtJS = jsGainNode
-- instance AudioNode a => JSArg a where
--   showtJS a = showtJS a

instance JSArg Int where
  showtJS = tshow

instance JSArg Double where
  showtJS = tshow
  
jsOscillatorNode :: OscillatorNode -> T.Text
jsOscillatorNode (OscillatorNode n _ _ _ _ _ _ _ _) = "sounds[" <> tshow n <> "]"

jsGainNode :: GainNode -> T.Text
jsGainNode (GainNode n _ _ _ _ _ _ ) = "sounds[" <> tshow n <> "]"

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

createGain :: Double -> WebAudio GainNode
createGain val = WebAudio $ procedure (CreateGain val)

-- start oscillator
start :: OscillatorNode -> WebAudio ()
start = WebAudio . command . Start

-- wait t seconds to start playing the oscillator
startWhen :: OscillatorNode -> Double -> WebAudio ()
startWhen o t = WebAudio . command $ StartWhen o t

-- stop oscillator
stop :: OscillatorNode -> WebAudio ()
stop = WebAudio . command . Stop

-- wait t seconds before stopping the oscillator
stopWhen :: OscillatorNode -> Double -> WebAudio ()
stopWhen o t = WebAudio . command $ StopWhen o t

-- connect n1 to n2. note: does not return reference to connected node like in js web audio api
-- connect :: AudioNode a => a -> a -> WebAudio ()
-- connect :: Nodes a => a -> a -> WebAudio ()
-- connect n1 n2 = WebAudio . command $ Connect n1 n2

connect :: AudioGraph AudNode b -> WebAudio ()
connect g = WebAudio . command $ Connect g

-- connects = audioGraph
-- connects :: (AudioNode a, AudioParam b) => [Either a b] -> Maybe AudioContext -> WebAudio ()
-- connects nodes ctx = WebAudio . command $ Connects (AudioGraph nodes ctx)

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
    Just a -> do -- is only commands
      putStrLn ""
      cmds <- handlePacket d pkt ""
      KC.send d cmds
      return a
    Nothing -> case pkt of
      AP.Command cmd -> do
        putStrLn ""
        cmds <- formatCommand cmd ""
        KC.send d cmds
      AP.Procedure p -> sendProcedure d p ""
      AP.Zip f g h   -> f <$> runAP d g <*> runAP d h
      AP.Pure p      -> pure p
      
  where
    handlePacket :: KC.Document -> ApplicativePacket Command Procedure a -> T.Text -> IO T.Text
    handlePacket doc pkt cmds =
      case pkt of
        AP.Command cmd -> formatCommand cmd cmds
        AP.Procedure p -> return cmds
        AP.Pure a      -> return cmds
        AP.Zip f g h   -> do
          gcmds <- handlePacket doc g cmds
          hcmds <- handlePacket doc h gcmds
          return hcmds
        
-- refactor to be easier to add stuff
sendProcedure :: KC.Document -> Procedure a -> T.Text -> IO a
sendProcedure d p@(CreateOscillator freq det nodetype) _ = do
  uq <- atomically getUniq
  KC.send d $ "CreateOscillator(" <> tshow freq <> "," <> tshow det <> ",'" <>
    tshow nodetype <> "')(" <> tshow uq <> ");"
  v <- KC.getReply d uq
  case parse (parseProcedure p) v of
    Error msg -> fail msg
    Success a -> return a
sendProcedure d p@(CreateGain val) _ = do
  uq <- atomically getUniq
  KC.send d $ "CreateGain(" <> tshow val <> ")(" <> tshow uq <> ");"
  v <- KC.getReply d uq
  case parse (parseProcedure p) v of
    Error msg -> fail msg
    Success a -> return a
    
-- sendToKC :: KC.Document -> T.Text -> IO a
-- sendToKC d t = do
--   uq <- atomically getUniq
--   KC.send d t
--   v <- KC.getReply d uq
--   case parse (parseProcedure p) v of
--     Error msg -> fail msg
--     Success a -> return a
  
parseProcedure :: Procedure a -> Value -> Parser a
parseProcedure (CreateOscillator {}) o = uncurry9 OscillatorNode <$> parseJSON o
parseProcedure (CreateGain {}) o = uncurry7 GainNode <$> parseJSON o

-- sendCommand :: KC.Document -> Command -> T.
-- sendCommand d (Start osc) = KC.send d $ "sounds[" <> tshow (index osc) <> "].start();"
-- sendCommand doc cmd = KC.send (formatCommand cmd)

formatCommand :: Command -> T.Text -> IO T.Text
formatCommand (Start osc) cmds       = return $ showtJS osc <> ".start();" <> cmds
formatCommand (StartWhen osc t) cmds = return $ showtJS osc <> ".start(" <> tshow t <> ");" <> cmds
formatCommand (Stop osc) cmds        = return $ showtJS osc <> ".stop();" <> cmds
formatCommand (StopWhen osc t) cmds  = return $ showtJS osc <> ".stop(" <> tshow t <> ");" <> cmds
formatCommand (Connect g) cmds = return $ audioGraphConnect g <> ";" <> cmds

audioGraphConnect :: AudioGraph AudNode b -> T.Text
audioGraphConnect (Node (AudNode a) g)  = showtJS a <> ".connect(" <> audioGraphConnect g  <> ")"
audioGraphConnect (EndNode (AudNode n)) = showtJS n
audioGraphConnect (EndParam p)          = showtJS p
audioGraphConnect (EndCtx c)            = showtJS c <> ".destination"

-- yeah it's gross
uncurry5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> (a1, a2, a3, a4, a5) -> b
uncurry5 f (a1, a2, a3, a4, a5) = f a1 a2 a3 a4 a5

uncurry7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b) -> (a1, a2, a3, a4, a5, a6, a7) -> b
uncurry7 f (a1, a2, a3, a4, a5, a6, a7) = f a1 a2 a3 a4 a5 a6 a7

uncurry8 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b) ->
            (a1, a2, a3, a4, a5, a6, a7, a8) -> b
uncurry8 f (a1, a2, a3, a4, a5, a6, a7, a8) = f a1 a2 a3 a4 a5 a6 a7 a8

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

instance FromJSON AudioParam where
  parseJSON = withText "AudioParam" $ \s -> return (read $ T.unpack s :: AudioParam)
    

-- instance FromJSON AudioParamType where
--   parseJSON = withText "AudioParamType" $ \p ->
--     case p of
--       "gain" -> return Gain
      
-- | OscillatorNode represents a periodic waveform with a frequency (in hertz), detuning (in cents), an OscillatorNodeType (e.g. a sine wave, square wave, etc.), etc.

data OscillatorNode = OscillatorNode {
  indexOsc                 :: !Int,
  frequencyOsc             :: !Double,
  detuneOsc                :: !Double,
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
