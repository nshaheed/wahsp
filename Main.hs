{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-} -- not sure which of these I actually need
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module: Web.Audio
License:
Maintainer: Nicholas Shaheed
Stability: Alpha

@wahsp@ is a binding for Haskell to the Web Audio API ala @blank-canvas@.
-}

module Web.Audio
  (
    Command(..)
  , AudioGraph(..)
  , AudNode(..)
  , connector
  , (.|.)
  , connectorLast
  , (.||.)
  , eNode
  , eParam
  , eCtx
  , Procedure(..)
  , AudioNode(..)
  , OscillatorNode(..)
  , GainNode(..)
  , AudioParam(..)
  , audioParamIdx
  , audioContext
  , ChannelCountMode
  , AudioParamType
  , showtJS
  , createOscillator
  , createGain
  , maxValue
  , minValue
  , value
  , currentTime
  , start
  , startWhen
  , stop
  , stopWhen
  , disconnect
  , disconnectOutput
  , disconnectOutputInput
  , disconnectDestNode
  , disconnectDestNodeSpec
  , disconnectDestParam
  , disconnectDestParamSpec
  , connect
  , setValue
  , setValueAtTime
  , linearRampToValueAtTime
  , exponentialRampToValueAtTime
  , setTargetAtTime
  , cancelScheduledValues
  , WebAudio(..)
  , send
  , sendApp
  , runAP
  , webAudio
  , WAOptions(..)
  , OscillatorNodeType
  ) where
-- WAhsP - Web Audio haskell Package

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
import Data.Monoid ((<>))
import qualified Data.Text as T

import Debug.Trace

import Network.Wai.Middleware.Static

import System.IO.Unsafe (unsafePerformIO)

import Text.Read
import qualified Text.Read.Lex as L

import qualified Web.Scotty.Comet as KC
import Web.Scotty

import System.IO.Unsafe (unsafePerformIO)

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
      -- osc2 <- createOscillator 2 0 Sine
      gain1 <- createGain 0.5

      val <- value (frequencyOsc osc1)
      def <- defaultValue (frequencyOsc osc1)
      max <- maxValue (frequencyOsc osc1)
      min <- minValue (frequencyOsc osc1)
      -- connecting an oscillator to another oscillator (or and audio source to any other
      -- audio source) doesn't work, no inlets
      let g = osc1 .|. gain1 .||. eCtx
      -- let g' = osc2 .||. eParam (gain gain1)
      traceShow val $ traceShow def $ traceShow max $ traceShow min $ connect g
      -- connect g'

      -- start osc1
      -- startWhen osc1 2

      -- let x = unsafePerformIO $ threadDelay (1000 * 1000)

      -- disconnectOutput osc1 -- only 0 is output
      -- disconnectDestParam osc1 (gain gain1) 
      -- stop osc1
      -- stopWhen osc1 5

      -- lfoEx
      setValueTests
      valueTests
      return ()

setValueTests = do
  osc1 <- createOscillator 200 0 Sine
  gain1 <- createGain 0.5

  setValue (frequencyOsc osc1) 800
  -- setValueAtTime (frequencyOsc osc1) 200 4
  -- linearRampToValueAtTime (frequencyOsc osc1) 400 8
  -- exponentialRampToValueAtTime (frequencyOsc osc1) 400 8
  setTargetAtTime (frequencyOsc osc1) 400 3 10
  cancelScheduledValues (frequencyOsc osc1) 2.9

  connect $ osc1 .|. gain1 .||. eCtx
  
  start osc1
  
valueTests = do
  osc1 <- createOscillator 200 0 Sine
  gain1 <- createGain 0.5

  val <- value (frequencyOsc osc1)
  def <- defaultValue (frequencyOsc osc1)
  max <- maxValue (frequencyOsc osc1)
  min <- minValue (frequencyOsc osc1)
  cur <- currentTime
  
  let g = osc1 .|. gain1 .||. eCtx
  
  traceShow cur $ traceShow val $ traceShow def $ traceShow max $ traceShow min $ connect g
  
oscillatorEx = do
  -- initialize an oscillator node and a gain node
  oscNode  <- createOscillator 440 0 Sine
  gainNode <- createGain 0.7

  -- connect the oscillator node to the gain node, and then connect the gain node to the
  -- audio context ( use (.||.) on the last value of the sequence of connections)
  connect (oscNode .|. gainNode .||. eCtx)

  -- start the oscillator node
  start oscNode

  -- alternatively, start the oscillator node with a five second delay
  -- startWhen oscNode 5.0
  
-- adapted from https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/connect(AudioParam)
lfoEx = do
  -- initialize the oscillator, the lfo, and the gain that will be controlled by the lso and
  -- the oscillator will be routed through
  osc1  <- createOscillator 400 0 Sine  
  lfo   <- createOscillator 2 0 Sine
  gain1 <- createGain 0.5

  
  connect (lfo .||. eParam (gain gain1)) -- connect the lfo to gain's param value
  connect (osc1 .||. eNode gain1)        -- connect the oscillator to the gain node
  connect (gain1 .||. eCtx)              -- connect the gain to the context

  -- start both the oscillators
  start osc1 
  start lfo
      

data Command :: * where
  Start                        :: OscillatorNode -> Command
  StartWhen                    :: OscillatorNode -> Double -> Command
  Stop                         :: OscillatorNode -> Command
  StopWhen                     :: OscillatorNode -> Double -> Command
  Connect                      :: AudioGraph AudNode b -> Command
  Disconnect                   :: AudioNode a => a -> Command
  DisconnectOutput             :: AudioNode a => a -> Int -> Command
  DisconnectOutputInput        :: AudioNode a => a -> a -> Int -> Int -> Command
  DisconnectDestNode           :: AudioNode a => a -> a -> Command
  DisconnectDestNodeSpec       :: AudioNode a => a -> a -> Int -> Command  
  DisconnectDestParam          :: AudioNode a => a -> AudioParam -> Command
  DisconnectDestParamSpec      :: AudioNode a => a -> AudioParam -> Int -> Command
  SetValue                     :: AudioParam -> Double -> Command
  SetValueAtTime               :: AudioParam -> Double -> Double -> Command
  LinearRampToValueAtTime      :: AudioParam -> Double -> Double -> Command
  ExponentialRampToValueAtTime :: AudioParam -> Double -> Double -> Command
  SetTargetAtTime              :: AudioParam -> Double -> Double -> Double -> Command
  CancelScheduledValues      :: AudioParam -> Double -> Command
  
data AudioGraph :: * -> * -> * where
  Node     :: AudNode -> AudioGraph AudNode b -> AudioGraph AudNode b
  EndNode  :: AudNode -> AudioGraph AudNode AudNode
  EndParam :: AudioParam -> AudioGraph AudNode AudioParam
  EndCtx   :: AudioContext -> AudioGraph AudNode AudioContext

data AudNode where
  AudNode :: (AudioNode a) => a -> AudNode

connector :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b
connector node = Node (AudNode node)

(.|.) :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b
(.|.) a b = connector a b
infix 7 .|.

connectorLast :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b  
connectorLast a b = Node (AudNode a) b

(.||.) :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b  
(.||.) = connectorLast

infix 8 .||.

-- eNode :: AudioNode a => a -> AudioNode AudNode AudNode
eNode :: AudioNode a => a -> AudioGraph AudNode AudNode
eNode a = EndNode (AudNode a)

eParam :: AudioParam -> AudioGraph AudNode AudioParam
eParam = EndParam

eCtx :: AudioGraph AudNode AudioContext
eCtx = EndCtx AudioContext

data Procedure     :: * -> * where
  CreateOscillator :: Double -> Double -> OscillatorNodeType -> Procedure OscillatorNode
  CreateGain       :: Double -> Procedure GainNode
  DefaultValue     :: AudioParam -> Procedure Double
  MaxValue         :: AudioParam -> Procedure Double
  MinValue         :: AudioParam -> Procedure Double
  Value            :: AudioParam -> Procedure Double
  CurrentTime      :: Procedure Double 

-- | And AudioNode is an interface for any audio processing module in the Web Audio API
class JSArg a => AudioNode a where
  numberOfInputs        :: a -> Int
  numberOfOutputs       :: a -> Int
  channelCount          :: a -> Int -- potentially change to maybe
  channelCountMode      :: a -> ChannelCountMode
  channelInterpretation :: a -> ChannelInterpretation 

-- | Instantizes OscillatorNode with the default values
instance AudioNode OscillatorNode where
  numberOfInputs        = numberOfInputsOsc
  numberOfOutputs       = numberOfOutputsOsc
  channelCount          = channelCountOsc
  channelCountMode      = channelCountModeOsc
  channelInterpretation = channelInterpretationOsc

instance AudioNode GainNode where
  numberOfInputs        = numberOfInputsGain
  numberOfOutputs       = numberOfOutputsGain
  channelCount          = channelCountGain
  channelCountMode      = channelCountModeGain
  channelInterpretation = channelInterpretationGain  

data AudioParam = AudioParam AudioParamType Int
  deriving (Read,Show,Eq)

audioParamIdx :: AudioParam -> Int
audioParamIdx (AudioParam _ i) = i

data GainNode = GainNode {
  indexGain                 :: !Int,
  gain                      :: !AudioParam,
  numberOfInputsGain        :: !Int,
  numberOfOutputsGain       :: !Int,
  channelCountGain          :: !Int,
  channelCountModeGain      :: !ChannelCountMode,
  channelInterpretationGain :: !ChannelInterpretation  
}
  deriving (Show, Read, Eq)

-- the audio context, this is pre-existing in the js, and only one is needed
data AudioContext = AudioContext
  deriving (Eq, Read)

audioContext = AudioContext

instance Show AudioContext where
  show AudioContext = "audioCtx"

data ChannelCountMode = Max | ClampedMax | Explicit
  deriving (Eq)

data AudioParamType = Gain | Frequency | Detune
  deriving (Eq,Read)

instance Show AudioParamType where
  show Gain = "gain"
  show Frequency = "frequency"
  
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
  showtJS (AudioParam ptype idx) = "sounds[" <> tshow idx <> "]." <> tshow ptype

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

defaultValue :: AudioParam -> WebAudio Double
defaultValue p = WebAudio $ procedure (DefaultValue p)

maxValue :: AudioParam -> WebAudio Double
maxValue p = WebAudio $ procedure (MaxValue p)

minValue :: AudioParam -> WebAudio Double
minValue p = WebAudio $ procedure (MinValue p)

value :: AudioParam -> WebAudio Double
value p = WebAudio $ procedure (Value p)

currentTime :: WebAudio Double
currentTime = WebAudio $ procedure (CurrentTime)

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

-- disconnect functions

-- disconnect all outgoing connections from AudioNode n
disconnect :: AudioNode a => a -> WebAudio ()
disconnect src = WebAudio . command $ Disconnect src

disconnectOutput :: AudioNode a => a -> Int -> WebAudio ()
disconnectOutput src idx = WebAudio . command $ DisconnectOutput src idx

disconnectOutputInput :: AudioNode a => a -> a -> Int -> Int -> WebAudio ()
disconnectOutputInput src dest output input = WebAudio . command $
                                              DisconnectOutputInput src dest output input
  
disconnectDestNode :: AudioNode a => a -> a -> WebAudio ()
disconnectDestNode src dest = WebAudio . command $ DisconnectDestNode src dest

disconnectDestNodeSpec :: AudioNode a => a -> a -> Int -> WebAudio ()
disconnectDestNodeSpec src dest idx = WebAudio . command $ DisconnectDestNodeSpec src dest idx

disconnectDestParam :: AudioNode a => a -> AudioParam -> WebAudio ()
disconnectDestParam src dest = WebAudio . command $ DisconnectDestParam src dest

disconnectDestParamSpec :: AudioNode a => a -> AudioParam -> Int -> WebAudio ()
disconnectDestParamSpec src dest idx = WebAudio . command $ DisconnectDestParamSpec src dest idx

connect :: AudioGraph AudNode b -> WebAudio ()
connect g = WebAudio . command $ Connect g

-- Set Value functions

setValue :: AudioParam -> Double -> WebAudio ()
setValue p val = WebAudio . command $ SetValue p val
  
setValueAtTime :: AudioParam -> Double -> Double -> WebAudio ()
setValueAtTime p val startTime = WebAudio . command $ SetValueAtTime p val startTime

linearRampToValueAtTime :: AudioParam -> Double -> Double -> WebAudio ()
linearRampToValueAtTime p val endTime = WebAudio . command $ LinearRampToValueAtTime  p val endTime

exponentialRampToValueAtTime :: AudioParam -> Double -> Double -> WebAudio ()
exponentialRampToValueAtTime p val endTime = WebAudio . command $
  ExponentialRampToValueAtTime p val endTime

setTargetAtTime :: AudioParam -> Double -> Double -> Double -> WebAudio ()
setTargetAtTime p target startTime timeConstant =
  WebAudio . command $ SetTargetAtTime  p target startTime timeConstant
  
cancelScheduledValues :: AudioParam -> Double -> WebAudio ()
cancelScheduledValues p startTime = WebAudio . command $ CancelScheduledValues p startTime

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
sendProcedure d p@(CreateOscillator freq det nodetype) _ =
  formatProcedure d p $ "CreateOscillator(" <> tshow freq <> "," <> tshow det <> ",'" <>
  tshow nodetype <> "')"
sendProcedure d p@(CreateGain val) _ = formatProcedure d p $ "CreateGain(" <> tshow val <> ")"
sendProcedure d p@(DefaultValue audioParam) _ =
  formatProcedure d p $ "DefaultValue(" <> showtJS audioParam <> ")"
sendProcedure d p@(MaxValue audioParam) _ =
  formatProcedure d p $ "MaxValue(" <> showtJS audioParam <> ")"
sendProcedure d p@(MinValue audioParam) _ =
  formatProcedure d p $ "MinValue(" <> showtJS audioParam <> ")"
sendProcedure d p@(Value audioParam) _ =
  formatProcedure d p $ "Value(" <> showtJS audioParam <> ")"
sendProcedure d p@(CurrentTime) _ = formatProcedure d p "GetCurrentTime()"
  
-- take text for function calls to be sent and add generate unique for port
formatProcedure :: KC.Document -> Procedure a -> T.Text -> IO a
formatProcedure d p call = do
  uq <- atomically getUniq
  KC.send d $ call <> "(" <> tshow uq <> ");"
  v <- KC.getReply d uq
  case parse (parseProcedure p) v of
    Error msg -> fail msg
    Success a -> return a

parseProcedure :: Procedure a -> Value -> Parser a
parseProcedure (CreateOscillator {}) o = uncurry9 OscillatorNode <$> parseJSON o
parseProcedure (CreateGain {}) o       = uncurry7 GainNode <$> parseJSON o
parseProcedure (DefaultValue {}) o     = parseJSON o
parseProcedure (MaxValue {}) o         = parseJSON o
parseProcedure (MinValue {}) o         = parseJSON o
parseProcedure (Value {}) o            = parseJSON o
parseProcedure (CurrentTime {}) o      = parseJSON o

formatCommand :: Command -> T.Text -> IO T.Text
formatCommand (Start osc) cmds       = return $ cmds <> showtJS osc <> ".start();"
formatCommand (StartWhen osc t) cmds = return $ cmds <> showtJS osc <> ".start(" <> tshow t <> ");"
formatCommand (Stop osc) cmds        = return $ cmds <> showtJS osc <> ".stop();"
formatCommand (StopWhen osc t) cmds  = return $ cmds <> showtJS osc <> ".stop(" <> tshow t <> ");"
formatCommand (Connect g) cmds       = return $ cmds <> audioGraphConnect g <> ";"
formatCommand (Disconnect src) cmds  = return $ cmds <> showtJS src <> ".disconnect();"
formatCommand (DisconnectOutput src idx) cmds = return $ cmds <> showtJS src <>
  ".disconnect(" <> showtJS idx <> ");"
formatCommand (DisconnectOutputInput src dest output input) cmds = return $
  cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> "," <> showtJS output <> "," <>
  showtJS input <> ");"
formatCommand (DisconnectDestNode src dest) cmds = return $
  cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> ");"
formatCommand (DisconnectDestNodeSpec src dest idx) cmds = return $
  cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> "," <> showtJS idx <> ");"
formatCommand (DisconnectDestParam src dest) cmds = return $
  cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> ");"
formatCommand (DisconnectDestParamSpec src dest idx) cmds = return $
  cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> "," <> showtJS idx <> ");"
formatCommand (SetValue p val) cmds = return $ cmds <> showtJS p <> ".value = " <> showtJS val <> ";"
formatCommand (SetValueAtTime p val startTime) cmds = return $ cmds <> showtJS p <>
  ".setValueAtTime(" <> showtJS val <> "," <> showtJS startTime <> ");"
formatCommand (LinearRampToValueAtTime p val endTime) cmds = return $ cmds <>
  showtJS p <> ".linearRampToValueAtTime(" <> showtJS val <> "," <> showtJS endTime <> ");"
formatCommand (ExponentialRampToValueAtTime p val endTime) cmds = return $ cmds <>
  showtJS p <> ".exponentialRampToValueAtTime(" <> showtJS val <> "," <> showtJS endTime <> ");"
formatCommand (SetTargetAtTime p target startTime timeConstant) cmds = return $ cmds <>
  showtJS p <> ".setTargetAtTime(" <> showtJS target <> "," <> showtJS startTime <> "," <>
  showtJS timeConstant <> ");"
formatCommand (CancelScheduledValues p startTime ) cmds = return $ cmds <>
  showtJS p <> ".cancelScheduledValues(" <> showtJS startTime <> ");"
  
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
          
-- | OscillatorNode represents a periodic waveform with a frequency (in hertz), detuning (in cents), an OscillatorNodeType (e.g. a sine wave, square wave, etc.), etc.

data OscillatorNode = OscillatorNode {
  indexOsc                 :: !Int,
  frequencyOsc             :: !AudioParam,
  detuneOsc                :: !AudioParam,
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
