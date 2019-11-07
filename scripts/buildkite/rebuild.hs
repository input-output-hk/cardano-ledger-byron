{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | To test this you can run:
--
-- > nix-shell .buildkite --run "ghci .buildkite/rebuild.hs"
--
-- TODO: adapt the instructions above with the correct path.

import           Build (LibraryName (LibraryName), Optimizations (Standard),
                     ShouldUploadCoverage (ShouldUploadCoverage),
                     StackExtraTestArgs (StackExtraTestArgs),
                     TestRun (TestRun), doBuild)
import           CommonBuild (Bool (True), CoverallsConfig (CoverallsConfig),
                     CoverallsTokenEnvVar (CoverallsTokenEnvVar),
                     ExtraShcArgs (ExtraShcArgs),
                     ExtraTixFilesDirectory (ExtraTixFilesDirectory), IO,
                     const, ($))


main :: IO ()
main =
  doBuild
    (LibraryName "cardano-ledger")
    Standard
    (ShouldUploadCoverage $ const True) -- TODO: change this __line__ to 'uploadCoverageIfBors'
    [TestRun $ StackExtraTestArgs $ const ["--ta", "--scenario=ContinuousIntegration"]]
    (CoverallsConfig
       (CoverallsTokenEnvVar "CARDANO_LEDGER_COVERALLS_REPO_TOKEN")
       (ExtraShcArgs ["--exclude-dirs", "crypto/test"])
       (ExtraTixFilesDirectory ".")
    )
