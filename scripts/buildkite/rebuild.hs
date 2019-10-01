{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Script for building @cardano-ledger@ with Stack under Buildkite.
--
-- The caching mechanism was taken from @cardano-wallet@.
--
-- To work on this script under GHCi, with Haskell dependencies provided, run:
--
-- >
--

import Prelude hiding (FilePath)

import Control.Exception ()
import Control.Monad.Trans.Maybe ()
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import Options.Applicative
  ( (<**>)
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , optional
  , progDesc
  , str
  )
import Safe ()
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Turtle
  ( FilePath
  , Text
  , (%)
  , d
  , echo
  , empty
  , eprintf
  , need
  , printf
  , proc
  , s
  , when
  )


-- | Run build and upload coverage information when successful
main :: IO ()
main = do
  (Options { cacheDirectory }) <- parseOptions

  buildResult <- buildStep (Just ["--scenario=ContinuousIntegration"])

  when (buildResult == ExitSuccess) coverageUploadStep

  exitWith buildResult


data Options = Options
    { cacheDirectory :: Maybe FilePath
    } deriving (Show)


parseOptions :: IO Options
parseOptions = execParser options
  where
    options = info (commandOptions <**> helper)
                   (fullDesc <> progDesc "Build cardano-ledger with stack in Buildkite")
      where
        commandOptions
          = fmap Options
          $ optional
          $ option
              (FP.decodeString <$> str)
              (  long "cache-dir"
              <> metavar "DIR"
              <> help "Location of project's cache"
              )

-- | Build and test all packages using stack
buildStep :: Maybe [Text] -> IO ExitCode
buildStep testArgs = do
  echo "+++ Build and test"
  run "stack" $ cfg ++ ["build", "--fast"] ++ buildArgs
 where
  cfg = ["--dump-logs", "--color", "always"]
  buildArgs =
    [ "--bench"
      , "--no-run-benchmarks"
      , "--haddock"
      , "--haddock-internal"
      , "--no-haddock-deps"
      , "--test"
      , "--coverage"
      ]
      ++ maybe [] ("--ta" :) testArgs


-- | Upload coverage information to coveralls
coverageUploadStep :: IO ()
coverageUploadStep = do
  echo "--- Uploading Coverage Information"
  need "CARDANO_LEDGER_COVERALLS_REPO_TOKEN" >>= \case
    Nothing -> printf
      "Missing coverall repo token. Not uploading coverage information.\n"
    Just repoToken -> do
      result <- proc
        "shc"
        [ "--repo-token"
        , repoToken
        , "--exclude-dirs"
        , "crypto/test"
        , "combined"
        , "all"
        ]
        empty
      case result of
        ExitSuccess   -> printf "Coverage information upload successful.\n"
        ExitFailure _ -> printf "Coverage information upload failed.\n"


run :: Text -> [Text] -> IO ExitCode
run cmd args = do
  printf (s % " " % s % "\n") cmd (T.unwords args)
  res <- proc cmd args empty
  case res of
    ExitSuccess      -> pure ()
    ExitFailure code -> eprintf
      ("error: Command exited with code " % d % "!\nContinuing...\n")
      code
  pure res
