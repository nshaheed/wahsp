{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Audio.JavaScript where

import Data.Aeson (FromJSON(..),Value(..),withText)
import Data.Aeson.Types (Parser,parse,Result(..))
import Data.Monoid ((<>))
import qualified Data.Text as T

import Text.Read
import qualified Text.Read.Lex as L

-- data types dealing directly with javascript

data AudioParam = AudioParam AudioParamType Int
  deriving (Read,Show,Eq)

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

instance Show AudioContext where
  show AudioContext = "audioCtx"

-- | How channels will be matched between connected inputs and output.
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/channelCountMode Detailed description.>
data ChannelCountMode :: * where
  Max        :: ChannelCountMode
  ClampedMax :: ChannelCountMode
  Explicit   :: ChannelCountMode
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


-- | Which type of 'AudioParam'
data AudioParamType =
  Gain |
  Frequency |
  Detune
  deriving (Eq,Read)

instance Show AudioParamType where
  show Gain = "gain"
  show Frequency = "frequency"
  
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

-- | OscillatorNode represents a periodic waveform with a frequency (in hertz), detuning (in cents), an OscillatorNodeType (e.g. a sine wave, square wave, etc.), etc.

data OscillatorNode = OscillatorNode {
  indexOsc                 :: !Int, -- ^ Index in javascript, for internal use
  frequencyOsc             :: !AudioParam, -- ^ the frequency of this oscilaltor
  detuneOsc                :: !AudioParam, -- ^ how many cents the 'frequencyOsc' is detuned by
  typeOsc                  :: !OscillatorNodeType, -- ^ periodic wave type
  numberOfInputsOsc        :: !Int, -- ^ number of inputs 
  numberOfOutputsOsc       :: !Int, -- ^ number of outputs
  channelCountOsc          :: !Int, -- ^ number of channels used when <https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Basic_concepts_behind_Web_Audio_API#Up-mixing_and_down-mixing up-mixing and down-mixing>
  channelCountModeOsc      :: !ChannelCountMode, -- ^ The 'ChannelCountMode' of this oscillator
  channelInterpretationOsc :: !ChannelInterpretation -- ^ The 'ChannelInterpretation' of this oscilaltor
}
  deriving (Read,Show,Eq)

-- classes for javascript obj
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

-- JSArg goes from data type javascript rep of that data
class JSArg a where
      -- | Display a value as JavaScript data.
      showtJS :: a -> T.Text

instance JSArg OscillatorNode where
  showtJS = jsOscillatorNode

jsOscillatorNode :: OscillatorNode -> T.Text
jsOscillatorNode (OscillatorNode n _ _ _ _ _ _ _ _) = "sounds[" <> tshow n <> "]"

instance JSArg AudioContext where
  showtJS a = tshow a

instance JSArg AudioParam where
  showtJS (AudioParam ptype idx) = "sounds[" <> tshow idx <> "]." <> tshow ptype

instance JSArg GainNode where
  showtJS = jsGainNode

jsGainNode :: GainNode -> T.Text
jsGainNode (GainNode n _ _ _ _ _ _ ) = "sounds[" <> tshow n <> "]"

instance JSArg Int where
  showtJS = tshow

instance JSArg Double where
  showtJS = tshow

tshow :: Show a => a -> T.Text
tshow a = T.pack $ show a

-- parseJSON
instance FromJSON ChannelCountMode where
  parseJSON = withText "ChannelCountMode" $ \s ->
    case s of
      "max"         -> return Max
      "clamped-max" -> return ClampedMax
      "explicit"    -> return Explicit
      _             -> fail "Parsing ChannelCountMode value failed: expected \"max\", \"clamped-max\", or \"Explicit\""


instance FromJSON ChannelInterpretation where
  parseJSON = withText "ChannelInterpretation" $ \s ->
    case s of
      "speakers" -> return Speakers
      "discrete" -> return Discrete
      _          -> fail "Parsing ChannelInterpretation value failed: expected \"speakers\", or \"discrete\""  


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
