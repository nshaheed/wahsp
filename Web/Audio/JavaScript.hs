{-# LANGUAGE OverloadedStrings #-}

module Web.Audio.JavaScript where

import Data.Aeson (FromJSON(..),Value(..),withText)
import Data.Aeson.Types (Parser,parse,Result(..))
import Data.Monoid ((<>))
import qualified Data.Text as T

import Text.Read
import qualified Text.Read.Lex as L

-- data types dealing directly with javascript

-- | An 'AudioParam' is an interface to a value that is related to the actual generation of audio,
-- it is usually related to a specific 'AudioNode'.  The 'Int' is for storing of the index for
-- retrieving in the javascript.
data AudioParam = AudioParam AudioParamType Int
  deriving (Read,Show,Eq)

-- | A 'GainNode' is an 'AudioNode' that controls the gain of a signal, you connect a sound source to
-- it and it will make that sound louder or softer. This actual value of the gain is usually between
-- \0.0\ and \1.0\.  \0.0\ will mute the sound, going higher than \1.0\ will increase the volume of the
-- sound.
data GainNode = GainNode {
  indexGain                 :: !Int, -- ^ Index of this particular node in the javascript, for internal use
  gain                      :: !AudioParam, -- ^ Handles the actual value of the gain
  numberOfInputsGain        :: !Int, -- ^ The number of inputs that the 'GainNode' has
  numberOfOutputsGain       :: !Int, -- ^ The number of outputs that the 'GainNode' has
  channelCountGain          :: !Int, -- ^ An 'Int' that determines how many channels are used when up-mixing and down-mixing connections
  channelCountModeGain      :: !ChannelCountMode, 
  channelInterpretationGain :: !ChannelInterpretation  -- ^ Specifies how channels are interpretated when up-mixing and down-mixing
}
  deriving (Show, Read, Eq)

-- | The audio context, this is pre-existing in the javascript, only one is needed, and this is a reference to it
data AudioContext = AudioContext
  deriving (Eq, Read)

instance Show AudioContext where
  show AudioContext = "audioCtx"

-- | How channels will be matched between connected inputs and output.
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/channelCountMode Detailed description.>
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


-- | Which type of 'AudioParam'
data AudioParamType = Gain | Frequency | Detune
  deriving (Eq,Read)

instance Show AudioParamType where
  show Gain      = "gain"
  show Frequency = "frequency"
  show Detune    = "detune"

-- | Specifies how channels are interpretated when up-mixing and down-mixing.
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/channelInterpretation Detailed description>
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

-- | What waveform is used in an 'OscillatorNode'
data OscillatorNodeType = Sine | Square | Sawtooth | Triangle | Custom -- ^ Not implemented yet
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

-- | An AudioNode is an interface for any audio processing module in the Web Audio API
class JSArg a => AudioNode a where
  numberOfInputs        :: a -> Int
  numberOfOutputs       :: a -> Int
  channelCount          :: a -> Int -- potentially change to maybe
  channelCountMode      :: a -> ChannelCountMode
  channelInterpretation :: a -> ChannelInterpretation

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

-- | JSArg goes from data type javascript representation of that data
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
