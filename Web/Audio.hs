{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: Web.Audio
License: BSD(see LICENSE file)
Maintainer: Nicholas Shaheed
Stability: Alpha

@wahsp@ (Web Audio HaSkell Protocol) is a binding for Haskell to the
<https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API Web Audio API> ala @blank-canvas@.
Audio sources, effects, etc. can be combined, manipulated, and otherwise controlled using haskell
and are then rendered in the the browser (see the above link for browser compatibility). 
-}

module Web.Audio
  (
    -- * Set-Up
    webAudio
  , WAOptions(..)
  , send  
  -- , AudioGraph(..)
  -- , AudNode(..)
  -- * Connecting nodes, params, and the audio context
  -- | The Web Audio API is comprised of nodes ('AudioNode's, 'AudioParam's, and the 'AudioContext')
  -- that are connected, input to output, to form a chain 
  -- comprised of sources, effects, and a destination.
  --
  -- This chain is typically organized as a /source -> effects -> destination/, where /destination/
  -- is either the 'AudioContext' (if you actually want to produce sound in this chain), some
  -- 'AudioParam' (if you want to control a param with an audio signal, e.g. a low-frequency
  -- oscillator (lfo)), or some 'AudioNode'.  
  --
  -- To chain together 'AudioNode's and 'AudioParam's, use '.|.' and end the chain with '.||.'
  -- For example:
  --
  -- @
  -- osc1  <- 'createOscillator' 200 0 'Sine'
  -- gain1 <- 'createGain' 0.5
  -- 
  -- 'connect' $ osc1 .|. gain1 .||. 'eCtx'
  -- 'start' osc1
  -- @
  -- See the <https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API official docs> for a more
  -- detailed overview.
  --
  , connect  
  , (.|.)
  , (.||.)
  , connector  
  , connectorLast  
  , eNode
  , eParam
  , eCtx
  -- * Data Types
  , WebAudio(..)  
  , AudioNode(..)
  , OscillatorNode(..)
  , OscillatorNodeType(..)    
  , GainNode(..)
  , AudioParam(..)
  , ChannelCountMode(..)
  , AudioParamType(..)
  -- , showtJS
  -- * Procedures
  -- ** Instantiation functions
  , createOscillator
  , createGain
  -- ** Other Procedures
  , audioContext    
  , maxValue
  , minValue
  , value
  , currentTime
  -- * Playback control
  -- Time in the api is the number of seconds since the session has been instantiated.
  -- ** 'OscillatorNode' specific playback controls
  , start
  , startWhen
  , stop
  , stopWhen
  -- ** Disconnecting functions
  , disconnect
  , disconnectOutput
  , disconnectOutputInput
  , disconnectDestNode
  , disconnectDestNodeSpec
  , disconnectDestParam
  , disconnectDestParamSpec
  -- * Change 'AudioParam' value
  -- | Different functions for altering the values of 'AudioParam's (immediately, ramping, etc.)
  , setValue
  , setValueAtTime
  , linearRampToValueAtTime
  , exponentialRampToValueAtTime
  , setTargetAtTime
  , cancelScheduledValues
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
-- import Control.Remote.Monad.Packet.Weak as WP
-- import Control.Remote.Monad.Packet.Strong as SP
import Control.Remote.Monad.Packet.Applicative as AP
-- import Control.Remote.Monad.Packet.Alternative as Alt
-- import qualified Data.Semigroup as SG

import Data.Aeson (FromJSON(..),Value(..),withText)
import Data.Aeson.Types (Parser,parse,Result(..))
import Data.Monoid ((<>))
import qualified Data.Text as T

-- import Debug.Trace

import Network.Wai.Middleware.Static

import Paths_wahsp

import System.IO.Unsafe (unsafePerformIO)

import Web.Audio.JavaScript
import Web.Audio.Packets
import Web.Audio.WebAudio

import qualified Web.Scotty.Comet as KC
import Web.Scotty

-- | 'webAudio' is the starting point for connecting and interacting with the API. A simple
-- example of how to use this is:
--
-- @
--  module Main where
--  import Web.Audio
--  
--  main :: IO ()
--  main = do
--    'webAudio' 3000 $ \doc -> do
--      'send' doc $ do
--        osc1  <- 'createOscillator' 200 0 'Sine' -- create an 'OscillatorNode'
--        gain1 <- 'createGain' 0.5              -- create a 'GainNode'
--  
--        'connect' $ osc1 '.|.' gain1 '.||.' 'eCtx'   -- connect these nodes together, and then connect them to the audio context
--  
--        'start' osc1 -- make sounds!
-- @
-- 
-- When running, go to <http://localhost:3000/> in a browser to hear a 200Hz sine wave!
--
-- More examples can be found <https://github.com/nshaheed/WebAudioHs/tree/master/examples here>.
webAudio :: WAOptions -> (KC.Document -> IO ()) -> IO ()
webAudio opts actions = do
  kcomet <- KC.kCometPlugin -- get comet file path
  dataDir <- getDataDir     -- get data (index.html, etc) file path

  putStrLn (dataDir ++ "/index.html")
  putStrLn (dataDir ++ "/static/kansas-comet.js")

  let pol = only [ ("",dataDir ++ "/index.html")
                   , ("js/kansas-comet.js",kcomet)
                   , ("js/jquery.js",dataDir ++ "/js/jquery.js")
                   , ("js/jquery-json.js",dataDir ++ "/js/jquery-json.js")                   
                   ]
            <|> (hasPrefix "js/") >-> addBase "."            

  let kcopts = KC.Options {KC.prefix = "/example", KC.verbose = if debug opts then 3 else 0}
  
  connectApp <- KC.connect kcopts $ \kc_doc -> do
    actions kc_doc

  scotty (port opts) $ do
    middleware $ staticPolicy pol
    connectApp
    
  return()


-- main :: IO ()
-- main = do
--   webAudio 3000 $ \doc -> do
--     send doc $ do
--       osc1 <- createOscillator 200 0 Sine
--       -- osc2 <- createOscillator 2 0 Sine
--       gain1 <- createGain 0.5

--       val <- value (frequencyOsc osc1)
--       def <- defaultValue (frequencyOsc osc1)
--       max <- maxValue (frequencyOsc osc1)
--       min <- minValue (frequencyOsc osc1)
--       -- connecting an oscillator to another oscillator (or and audio source to any other
--       -- audio source) doesn't work, no inlets
--       let g = osc1 .|. gain1 .||. eCtx
--       -- let g' = osc2 .||. eParam (gain gain1)
--       -- traceShow val $ traceShow def $ traceShow max $ traceShow min $
--       connect g
--       -- connect g'

--       -- start osc1
--       -- startWhen osc1 2

--       -- let x = unsafePerformIO $ threadDelay (1000 * 1000)

--       -- disconnectOutput osc1 -- only 0 is output
--       -- disconnectDestParam osc1 (gain gain1) 
--       -- stop osc1
--       -- stopWhen osc1 5

--       -- lfoEx
--       setValueTests
--       valueTests
--       return ()

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
  
  -- traceShow cur $ traceShow val $ traceShow def $ traceShow max $ traceShow min $
  connect g
  
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

-- | Connect the front of the chain of nodes together, end the chain with '.||.'
(.|.) :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b
(.|.) a b = connector a b
infix 7 .|.

-- | End the chain of 'AudioNode's.
--
-- To end the chain at the audio context:
--  
-- > connect $ osc1 .|. gain1 .||. eCtx
--
-- To end with an 'AudioParam' (that is located in the 'AudioNode' /gain1/):
--
-- > connect $ osc1 .|. gain1 .||. eParam (gain gain1)
--
-- To end with the 'AudioNode' /gain1/:
--
-- > connect $ osc1 .|. gain1 .||. eNode gain1
(.||.) :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b  
(.||.) = connectorLast

-- | function implementation of '.|.'
connector :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b
connector node = Node (AudNode node)

-- | function implementation of '.||.'
connectorLast :: forall b a. AudioNode a => a -> AudioGraph AudNode b -> AudioGraph AudNode b  
connectorLast a b = Node (AudNode a) b

infix 8 .||.

-- | Set the ending node to an 'AudioNode'
eNode :: AudioNode a => a -> AudioGraph AudNode AudNode
eNode a = EndNode (AudNode a)

-- | Set the ending node to an 'AudioParam'
eParam :: AudioParam -> AudioGraph AudNode AudioParam
eParam = EndParam

-- | Set the ending node to the 'AudioContext' 
eCtx :: AudioGraph AudNode AudioContext
eCtx = EndCtx AudioContext

-- TODO: decide if it's appropriate to have this exposed
-- audioParamIdx :: AudioParam -> Int
-- audioParamIdx (AudioParam _ i) = i

-- | A function that returns an 'AudioContext'
audioContext = AudioContext

-- | creates an oscillator with a frequency (in hertz), a detuning value (in cents), and an 'OscillatorNodeType' (e.g. a sine wave, square wave, etc.)
createOscillator :: Double -- ^ Frequency (in hertz)
                 -> Double -- ^ Detuning (in cents)
                 -> OscillatorNodeType -- ^ Waveform type
                 -> WebAudio OscillatorNode 
createOscillator freq det osctype = WebAudio $ procedure (CreateOscillator freq det osctype)

-- | Create a gain node with a gain value, typically between /0.0/ and /1.0/
createGain :: Double -> WebAudio GainNode
createGain val = WebAudio $ procedure (CreateGain val)

-- | Get the default value of an 'AudioParam' (this could vary from browser to browser)
defaultValue :: AudioParam -> WebAudio Double
defaultValue p = WebAudio $ procedure (DefaultValue p)

-- | Get the maximum value of an 'AudioParam'
maxValue :: AudioParam -> WebAudio Double
maxValue p = WebAudio $ procedure (MaxValue p)

-- | Get the minimum value of an 'AudioParam'
minValue :: AudioParam -> WebAudio Double
minValue p = WebAudio $ procedure (MinValue p)

-- | Get the current value of an 'AudioParam'
value :: AudioParam -> WebAudio Double
value p = WebAudio $ procedure (Value p)

-- | Get the current time in the sessions (in seconds).  This represents the amount of time that has
-- passed since the session was instantiated
currentTime :: WebAudio Double
currentTime = WebAudio $ procedure (CurrentTime)

-- | Immediately start playback of an 'OscillatorNode' 
start :: OscillatorNode -> WebAudio ()
start = WebAudio . command . Start

-- | Start playing an 'OscillatorNode' at \t\ seconds.  If \t\ has already passed, it will immediately stop
startWhen :: OscillatorNode -> Double -> WebAudio ()
startWhen o t = WebAudio . command $ StartWhen o t

-- | Immediately stop playback of an 'OscillatorNode'
stop :: OscillatorNode -> WebAudio ()
stop = WebAudio . command . Stop

-- | Stop playing an 'OscillatorNode' at \t\ seconds.  If \t\ has already passed, it will immediately stop
stopWhen :: OscillatorNode -> Double -> WebAudio ()
stopWhen o t = WebAudio . command $ StopWhen o t

-- disconnect functions

-- | Disconnect all outgoing connections from AudioNode n
disconnect :: AudioNode a => a -> WebAudio ()
disconnect src = WebAudio . command $ Disconnect src

-- | Disconnect a specific output 
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

-- | Connects the 'AudioGraph' chain (made by connecting 'AudioNode's, 'AudioParam's, and 'AudioContext's
-- with '.|.' and '.||.')
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
  
{-# NOINLINE uniqVar #-}
uniqVar :: TVar Int
uniqVar = unsafePerformIO $ newTVarIO 0

getUniq :: STM Int
getUniq = do
      u <- readTVar uniqVar
      writeTVar uniqVar (u + 1)
      return u
