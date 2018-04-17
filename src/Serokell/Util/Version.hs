module Serokell.Util.Version
       ( -- * Git revision
         retrieveGitRev
       ) where

import Universum

import Instances.TH.Lift ()
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

{- | Gets the git revision.
For example, you can use it configuring CLI options:

@
versionOption = infoOption
    ("Git revision: " <> toString $(retrieveGitRev))
    (long "version" <> help "Show version")
@

You'll need @-XTemplateHaskell@.

Also note, that in order to see the latest git revision for the latest commit
you should force recompilation of the file which contains @$(retrieveGitRev)@.
-}
retrieveGitRev :: TH.Q TH.Exp
retrieveGitRev = do
    cti <- TH.runIO $ Text.strip . fromString <$> retrieveGit
    TH.lift cti
  where
    retrieveGit :: IO String
    retrieveGit = whenNothingM (lookupEnv "GITREV") retrieveFromGitExecutable

    retrieveFromGitExecutable :: IO String
    retrieveFromGitExecutable = do
        (exitCode, output, _) <-
            readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
        pure $ case exitCode of
            ExitSuccess -> output
            _           -> fail "Couldn't retrieve 'git' revision"
