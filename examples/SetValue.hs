-- Ramps the 200hz sine wave to 800hz over 8 seconds

module Main where

import Web.Audio

main :: IO ()
main = do
  webAudio 3000 $ \doc -> do
    send doc $ do
      osc1 <- createOscillator 200 0 Sine
      gain1 <- createGain 0.5

      linearRampToValueAtTime (frequencyOsc osc1) 800 8

      connect $ osc1 .|. gain1 .||. eCtx
  
      start osc1
