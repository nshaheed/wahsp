{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Web.Audio.WebAudio where

import Web.Audio.JavaScript

-- TODO: add     middleware :: [KC.Middleware],    
-- | Various options when sending info to the browser
data WAOptions = WAOptions
  { port          :: Int,    -- ^ which port to send to
    events        :: [Int],  -- ^ not implemented yet
    debug         :: Bool,   -- ^ turns on debugging, defaults to 'False'
    root          :: String, -- ^ location of static files
    weak          :: Bool    -- ^ not implemented yet
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

data AudioGraph :: * -> * -> * where
  Node     :: AudNode -> AudioGraph AudNode b -> AudioGraph AudNode b
  EndNode  :: AudNode -> AudioGraph AudNode AudNode
  EndParam :: AudioParam -> AudioGraph AudNode AudioParam
  EndCtx   :: AudioContext -> AudioGraph AudNode AudioContext

data AudNode where
  AudNode :: (AudioNode a) => a -> AudNode
