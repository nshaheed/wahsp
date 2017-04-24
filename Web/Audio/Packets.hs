{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Audio.Packets where

import Control.Monad(liftM2)
-- import Control.Remote.Monad
import Control.Remote.WithAsync.Monad

import Data.Monoid ((<>))
import qualified Data.Semigroup as SG
import qualified Data.Text as T

import Web.Audio.JavaScript
import Web.Audio.WebAudio

import Debug.Trace

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

data Procedure     :: * -> * where
  CreateOscillator :: Double -> Double -> OscillatorNodeType -> Procedure OscillatorNode
  CreateGain       :: Double -> Procedure GainNode
  DefaultValue     :: AudioParam -> Procedure Double
  MaxValue         :: AudioParam -> Procedure Double
  MinValue         :: AudioParam -> Procedure Double
  Value            :: AudioParam -> Procedure Double
  CurrentTime      :: Procedure Double 

-- | Contains the commands and procedures to be sent to the web browser
newtype WebAudio a = WebAudio (RemoteMonad Command Procedure a)
  deriving (Functor, Applicative, Monad)

instance SG.Semigroup a => SG.Semigroup (WebAudio a) where
  (<>) = liftM2 (SG.<>)

instance Monoid a => Monoid (WebAudio a) where
  mappend = liftM2 mappend
  mempty  = return mempty
  
audioGraphConnect :: AudioGraph AudNode b -> T.Text
-- because the api doesn't allow chaining, you can't compose the connect calls
audioGraphConnect (Node (AudNode a) g@(Node (AudNode b) _))  =
  showtJS a <> ".connect(" <> showtJS b <> ");" <> audioGraphConnect g
audioGraphConnect (Node (AudNode a) g)  =
  -- traceShow (showtJS a <> ".connect(" <> audioGraphConnect g  <> ")") $ 
  -- showtJS a <> ".connect(" <> audioGraphConnect g  <> ")"
  showtJS a <> ".connect(" <> audioGraphConnect g  <> ")"  
audioGraphConnect (EndNode (AudNode n)) =
  -- traceShow (showtJS n) $
  showtJS n
audioGraphConnect (EndParam p)          =
  -- traceShow (showtJS p) $
  showtJS p
audioGraphConnect (EndCtx c)            =
  -- traceShow (showtJS c <> ".destination") $
  showtJS c <> ".destination"
