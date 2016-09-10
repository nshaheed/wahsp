{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Audio.Packets where

import Control.Monad(liftM2)
import Control.Remote.Monad

import Data.Monoid ((<>))
import qualified Data.Semigroup as SG
import qualified Data.Text as T

import Web.Audio.JavaScript
import Web.Audio.WebAudio

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

newtype WebAudio a = WebAudio (RemoteMonad Command Procedure a)
  deriving (Functor, Applicative, Monad)

instance SG.Semigroup a => SG.Semigroup (WebAudio a) where
  (<>) = liftM2 (SG.<>)

instance Monoid a => Monoid (WebAudio a) where
  mappend = liftM2 mappend
  mempty  = return mempty

-- -- refactor to be easier to add stuff
-- sendProcedure :: KC.Document -> Procedure a -> T.Text -> IO a
-- sendProcedure d p@(CreateOscillator freq det nodetype) _ =
--   formatProcedure d p $ "CreateOscillator(" <> tshow freq <> "," <> tshow det <> ",'" <>
--   tshow nodetype <> "')"
-- sendProcedure d p@(CreateGain val) _ = formatProcedure d p $ "CreateGain(" <> tshow val <> ")"
-- sendProcedure d p@(DefaultValue audioParam) _ =
--   formatProcedure d p $ "DefaultValue(" <> showtJS audioParam <> ")"
-- sendProcedure d p@(MaxValue audioParam) _ =
--   formatProcedure d p $ "MaxValue(" <> showtJS audioParam <> ")"
-- sendProcedure d p@(MinValue audioParam) _ =
--   formatProcedure d p $ "MinValue(" <> showtJS audioParam <> ")"
-- sendProcedure d p@(Value audioParam) _ =
--   formatProcedure d p $ "Value(" <> showtJS audioParam <> ")"
-- sendProcedure d p@(CurrentTime) _ = formatProcedure d p "GetCurrentTime()"

  
-- -- take text for function calls to be sent and add generate unique for port
-- formatProcedure :: KC.Document -> Procedure a -> T.Text -> IO a
-- formatProcedure d p call = do
--   uq <- atomically getUniq
--   KC.send d $ call <> "(" <> tshow uq <> ");"
--   v <- KC.getReply d uq
--   case parse (parseProcedure p) v of
--     Error msg -> fail msg
--     Success a -> return a

-- parseProcedure :: Procedure a -> Value -> Parser a
-- parseProcedure (CreateOscillator {}) o = uncurry9 OscillatorNode <$> parseJSON o
-- parseProcedure (CreateGain {}) o       = uncurry7 GainNode <$> parseJSON o
-- parseProcedure (DefaultValue {}) o     = parseJSON o
-- parseProcedure (MaxValue {}) o         = parseJSON o
-- parseProcedure (MinValue {}) o         = parseJSON o
-- parseProcedure (Value {}) o            = parseJSON o
-- parseProcedure (CurrentTime {}) o      = parseJSON o

-- formatCommand :: Command -> T.Text -> IO T.Text
-- formatCommand (Start osc) cmds       = return $ cmds <> showtJS osc <> ".start();"
-- formatCommand (StartWhen osc t) cmds = return $ cmds <> showtJS osc <> ".start(" <> tshow t <> ");"
-- formatCommand (Stop osc) cmds        = return $ cmds <> showtJS osc <> ".stop();"
-- formatCommand (StopWhen osc t) cmds  = return $ cmds <> showtJS osc <> ".stop(" <> tshow t <> ");"
-- formatCommand (Connect g) cmds       = return $ cmds <> audioGraphConnect g <> ";"
-- formatCommand (Disconnect src) cmds  = return $ cmds <> showtJS src <> ".disconnect();"
-- formatCommand (DisconnectOutput src idx) cmds = return $ cmds <> showtJS src <>
--   ".disconnect(" <> showtJS idx <> ");"
-- formatCommand (DisconnectOutputInput src dest output input) cmds = return $
--   cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> "," <> showtJS output <> "," <>
--   showtJS input <> ");"
-- formatCommand (DisconnectDestNode src dest) cmds = return $
--   cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> ");"
-- formatCommand (DisconnectDestNodeSpec src dest idx) cmds = return $
--   cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> "," <> showtJS idx <> ");"
-- formatCommand (DisconnectDestParam src dest) cmds = return $
--   cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> ");"
-- formatCommand (DisconnectDestParamSpec src dest idx) cmds = return $
--   cmds <> showtJS src <> ".disconnect(" <> showtJS dest <> "," <> showtJS idx <> ");"
-- formatCommand (SetValue p val) cmds = return $ cmds <> showtJS p <> ".value = " <> showtJS val <> ";"
-- formatCommand (SetValueAtTime p val startTime) cmds = return $ cmds <> showtJS p <>
--   ".setValueAtTime(" <> showtJS val <> "," <> showtJS startTime <> ");"
-- formatCommand (LinearRampToValueAtTime p val endTime) cmds = return $ cmds <>
--   showtJS p <> ".linearRampToValueAtTime(" <> showtJS val <> "," <> showtJS endTime <> ");"
-- formatCommand (ExponentialRampToValueAtTime p val endTime) cmds = return $ cmds <>
--   showtJS p <> ".exponentialRampToValueAtTime(" <> showtJS val <> "," <> showtJS endTime <> ");"
-- formatCommand (SetTargetAtTime p target startTime timeConstant) cmds = return $ cmds <>
--   showtJS p <> ".setTargetAtTime(" <> showtJS target <> "," <> showtJS startTime <> "," <>
--   showtJS timeConstant <> ");"
-- formatCommand (CancelScheduledValues p startTime ) cmds = return $ cmds <>
--   showtJS p <> ".cancelScheduledValues(" <> showtJS startTime <> ");"
  
audioGraphConnect :: AudioGraph AudNode b -> T.Text
audioGraphConnect (Node (AudNode a) g)  = showtJS a <> ".connect(" <> audioGraphConnect g  <> ")"
audioGraphConnect (EndNode (AudNode n)) = showtJS n
audioGraphConnect (EndParam p)          = showtJS p
audioGraphConnect (EndCtx c)            = showtJS c <> ".destination"
