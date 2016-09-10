module Main where

import Web.Audio

main :: IO ()
main = do
  webAudio 3000 $ \doc -> do
    send doc $ do
      osc1  <- createOscillator 200 0 Sine
      gain1 <- createGain 0.5

      -- Note: connecting an oscillator to another oscillator (or and audio source to any other
      -- audio source) doesn't work, as audio sources do not have any inlets

      connect $ osc1 .|. gain1 .||. eCtx

      start osc1
