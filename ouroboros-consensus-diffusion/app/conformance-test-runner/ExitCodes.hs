{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Custom exit codes to implement the shrinking logic externally.
module ExitCodes (ExitStatus (.., Success), StatusFlag (..), exitWithStatus) where

import Data.Bits (Ior (..))
import Data.Set (Set)
import System.Exit (ExitCode (..), exitWith)

-- | Exit statuses for the test runner. 'Success' is represented
-- by an empty set of 'StatusFlags'.
data ExitStatus = InternalError | BadUsage | Flags (Set StatusFlag)

pattern Success :: ExitStatus
pattern Success <- Flags (null -> True)
  where
    Success = Flags mempty

-- | A 'ContinueShrinking' flag is returned whenever the 'TestFailed' or got
-- 'Success' with a non-empty shrink index as input, unless no more shrinking
-- on the input is possible. It is intended to signal the user to manually
-- /pump/ the shrinker.
data StatusFlag = TestFailed | ContinueShrinking deriving (Eq, Ord)

exitWithStatus :: ExitStatus -> IO a
exitWithStatus = exitWith . \case
  Success -> ExitSuccess
  InternalError -> ExitFailure 1
  BadUsage -> ExitFailure 2
  -- Flags are combined using bit-wise OR.
  Flags flags -> ExitFailure $ getIor $ foldMap flagToCode flags
 where
  flagToCode :: StatusFlag -> Ior Int
  flagToCode = Ior . \case
    TestFailed -> 4
    ContinueShrinking -> 8
