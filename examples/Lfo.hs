-- A low frequency oscillator making a tremolo effect (oscillating the gain)

module Main where

import Web.Audio

main :: IO ()
main = do
  webAudio 3000 $ \doc -> do
    send doc $ do  
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
