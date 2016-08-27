{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-} -- not sure which of these I actually need
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- what I want to do:
-- send over to js (map to web audio functionality)

-- first step: connect to js

import Control.Natural
import Control.Remote.Monad
import Control.Remote.Monad.Packet.Weak as WP
import Control.Remote.Monad.Packet.Strong as SP
import Control.Remote.Monad.Packet.Applicative as AP
import Control.Remote.Monad.Packet.Alternative as Alt

import Data.Char
import Data.Default.Class
import qualified Data.Text as T

import Network.Wai.Middleware.Static

import qualified Web.Scotty.Comet as KC
import Web.Scotty

data Command :: * where
  CreateOscillator :: OscillatorNode -> Command

data Procedure :: * -> * where
  Dummy :: Procedure Int

class AudioNode a where
  jsAudioNode :: a -> T.Text
  numberOfInputs :: a -> Int
  numberOfOutputs :: a -> Int
  channelCount :: a -> Int -- potentially change to maybe
  channelCountMode :: a -> ChannelCountMode
  channelInterpretation :: a -> ChannelInterpretation 

data ChannelCountMode = Max | ClampedMax | Explicit
  deriving (Eq)

instance Show ChannelCountMode where
  show Max        = "max"
  show ClampedMax = "clamped-max"
  show Explicit   = "explicit"

data ChannelInterpretation = Speakers | Discrete
  deriving (Eq)

instance Show ChannelInterpretation where
  show Speakers = "speakers"
  show Discrete = "discrete"
  
createOscillator :: OscillatorNode -> RemoteMonad Command Procedure ()
createOscillator o = command (CreateOscillator o)

newtype WebAudio a = WebAudio (RemoteMonad Command Procedure a)
  deriving (Functor, Applicative, Monad)

sendApp :: KC.Document -> WebAudio a -> IO a
sendApp d (WebAudio m) = (run $ runMonad $ nat (runAp d)) m

-- runAp :: KC.Document -> WebAudio a -> IO a
runAp :: KC.Document -> ApplicativePacket Command Procedure a -> IO a
runAp d pkt =
  case AP.superCommand pkt of
    Just a -> do
      let cmds = formatPackets pkt ""
      KC.send d cmds
      return a
    Nothing -> case pkt of
      AP.Command cmd -> do KC.send d (formatCommand cmd)
      

  where
    formatPackets :: ApplicativePacket Command Procedure a -> T.Text -> T.Text
    formatPackets pakt cmds = T.unlines[formatPacket pakt,cmds]
      
    formatPacket :: ApplicativePacket Command Procedure a -> T.Text
    formatPacket pakt =
      case pakt of
        AP.Command cmd -> formatCommand cmd
        AP.Procedure (Dummy) -> " "

    formatCommand :: Command -> T.Text
    formatCommand (CreateOscillator (OscillatorNode freq det wave)) =
      T.unlines [T.concat ["osc.frequency.value = ", T.pack . show $ freq, ";"],
                 T.concat ["osc.detune.value = ",T.pack . show $ det,";"],
                 T.concat ["osc.type = '" , T.pack . show $ wave , "';"]
                ]
-- runWA d (RemoteMonad Command Procedure a ) =
  
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

main :: IO ()
main = do
  kcomet <- KC.kCometPlugin

  let pol = only [ ("","index.html")
                 , ("js/kansas-comet.js",kcomet)
                 ]
        <|> (hasPrefix "js/") >-> addBase "."

  connectApp <- KC.connect opts test

  scotty 3000 $ do
    middleware $ staticPolicy pol
    connectApp
    
  return()

opts :: KC.Options
opts = def { KC.prefix = "/example"}
-- opts = KC.Options { prefix = "/example"}

test :: KC.Document -> IO ()
test doc = do
  putStrLn "in test"
  -- KC.send doc "testOsc()"
  -- oscType doc Sawtooth
  -- KC.send doc "osc.start();" -- both work
  -- sendApp doc (WebAudio (createOscillator (oscillatorNode 220 0 Square)))
  -- KC.send doc "osc.start();" -- both work
  -- KC.send doc "console.log(\"test\");"
  -- KC.send doc "var osc1 = audioCtx.createOscillator();"
  -- KC.send doc (T.unlines ["osc1.frequency.value = 350;",
  --                        "console.log(osc1.frequency.value);",
  --                        "osc1.start();"
  --                        ])
  -- KC.send doc "testE();"
  -- KC.send doc "osc.start();"
  -- KC.send doc "console.log(osc1.frequency.value;"
  return ()

oscType :: KC.Document -> OscillatorNodeType -> IO ()
oscType doc t = do
  KC.send doc (T.concat ["osc.type = '" , T.pack . show $ t , "';"])
  -- KC.send doc "osc.type = 'square';"
  
data OscillatorNodeType = Sine | Square | Sawtooth | Triangle | Custom
     deriving (Read, Eq)

instance Show OscillatorNodeType where
  show Sine     = "sine"
  show Square   = "square"
  show Sawtooth = "sawtooth"
  show Triangle = "triangle"
  show Custom   = "custom"

data OscillatorNode = OscillatorNode !Double !Double !OscillatorNodeType
     deriving (Read, Show, Eq)

oscillatorNode :: Double -> Double -> OscillatorNodeType -> OscillatorNode
oscillatorNode = OscillatorNode

-- start :: OscillatorNode -> Double -> IO ()

-- sendToCanvas :: DeviceContext -> Instr -> IO ()
-- sendToCanvas cxt cmds = do
--       KC.send (theComet cxt) . toStrict . toLazyText $ surround "syncToFrame(function(){"  "});" <> cmds
