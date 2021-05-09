{-# LANGUAGE RecordWildCards, ViewPatterns, DataKinds #-}
{-# LANGUAGE NoStarIsType, TypeOperators, TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
import           Clash.Prelude           hiding ( filter )

import           Clash.Shake
import           Clash.Shake.Symbiflow

import           Control.Monad
-- import Text.Regex.Applicative
-- import Text.Regex.Applicative.Common
-- import Data.Filtrable
import           Data.Maybe                     ( fromMaybe )
import           Data.Traversable               ( for )
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath

targets = [("iCESugar-Pro-v1.3", symbiflowECP5 iCESugar_Pro_v1_3)]

outDir = ".build"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = outDir } $ do
  useConfig "build.mk"

  phony "clean" $ do
    putNormal $ "Cleaning files in " <> outDir
    removeFilesAfter outDir ["//*"]

  kit@ClashKit {..} <- clashRules
    (outDir </> "clash")
    SystemVerilog
    ["src"]
    "Blink"
    ["-Wno-partial-type-signatures", "-fclash-clear"]
    (pure ())

  forM_ targets $ \(name, synth) -> do
    SynthKit {..} <- synth kit
                           (outDir </> name </> "synth")
                           ("target" </> name)
                           "blink"

    mapM_ (uncurry $ nestedPhony name) $ ("bitfile", need [bitfile]) : phonies

  phony "clashi" $ clash ["--interactive", "src/Blink.hs"]

