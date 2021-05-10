{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, ViewPatterns, DataKinds #-}
{-# LANGUAGE NoStarIsType, TypeOperators, TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import           Clash.Prelude           hiding ( filter )

import           Clash.Shake
import           Clash.Shake.Symbiflow

import           Data.Aeson
import           Text.Mustache
import qualified Text.Mustache.Compile.TH      as TH

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Traversable               ( for )
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath

targets = [("iCESugar-Pro-v1.3", symbiflowECP5 iCESugar_Pro_v1_3)]

outDir = ".build"

srcDirs = ["src"]

ghcArgs = ["-Wno-partial-type-signatures"]

clashArgs = ["-fclash-clear"]

top :: String
top = "hdmi"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = outDir } $ do
  useConfig "build.mk"

  phony "clean" $ do
    putNormal $ "Cleaning files in " <> outDir
    removeFilesAfter outDir ["//*"]

  kit@ClashKit {..} <- clashRules (outDir </> "clash")
                                  Verilog
                                  srcDirs
                                  "HDMITop"
                                  (ghcArgs <> clashArgs)
                                  (pure ())

  forM_ targets $ \(name, synth) -> do
    SynthKit {..} <- synth kit
                           (outDir </> name </> "synth")
                           ("target" </> name)
                           top

    mapM_ (uncurry $ nestedPhony name) $ ("bitfile", need [bitfile]) : phonies

  outDir </> "*.sby" %> \out -> do
    srcs <- manifestSrcs
    need srcs
    let template = $(TH.compileMustacheFile "template/sby.sby.mustache")
    let values =
          object
            . mconcat
            $ [ ["sources" .= T.pack (unwords (takeFileName <$> srcs))]
              , ["files" .= T.pack (unlines srcs)]
              , ["top" .= T.pack top]
              ]
    writeFileChanged out . TL.unpack $ renderMustache template values

  "hie.yaml" %> \out -> do
    let
      json = encode $ object
        [ "cradle" .= object
            [ "multi"
                .= (  [ object
                          [ "path" .= ("Shakefile.hs" :: String)
                          , "config" .= object
                            [ "cradle"
                                .= object
                                     [ "direct" .= object
                                         ["arguments" .= ([] :: [String])]
                                     ]
                            ]
                          ]
                      ]
                   <> [ object
                          [ "path" .= ("./" <> p)
                          , "config" .= object
                            [ "cradle" .= object
                                [ "direct"
                                    .= object
                                         [ "arguments"
                                             .= (  [ "-i" <> src
                                                   | src <- srcDirs
                                                   ]
                                                <> ghcArgs
                                                )
                                         ]
                                ]
                            ]
                          ]
                      | p <- srcDirs
                      ]
                   )
            ]
        ]
    writeFileChanged out . BSL.unpack $ json

  phony "prove" $ do
    let sby = outDir </> top <.> "sby"
    need [sby]
    cmd_ ("sby" :: String) ["-f", sby]

  phony "clashi" $ clash ["--interactive", "src/HDMITop.hs"]
