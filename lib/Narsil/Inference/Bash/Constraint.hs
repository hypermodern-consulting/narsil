{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                            // infer // constraint
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "At least, he thought, as the duster's angry gibberish faded behind him,
--    the gangs gave you some structure. If you were Gothick and the Kasuals
--    chopped you out, it made sense. Maybe the ultimate reasons behind it
--    were crazy, but there were rules."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // facts to // constraints
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Bash.Constraint (
  factsToConstraints,
  factToConstraints,
)
where

import Narsil.Bash.Builtins (lookupArgType)
import Narsil.Bash.Types

-- | Convert all facts to constraints
factsToConstraints :: [Fact] -> [Constraint]
factsToConstraints = concatMap factToConstraints

-- | Convert a single fact to constraints
factToConstraints :: Fact -> [Constraint]
factToConstraints (DefaultIs variable literal _) =
  [TVar (TypeVar variable) :~: literalType literal]
factToConstraints (DefaultFrom variable otherVariable _) =
  [TVar (TypeVar variable) :~: TVar (TypeVar otherVariable)]
factToConstraints (AssignFrom variable otherVariable _) =
  [TVar (TypeVar variable) :~: TVar (TypeVar otherVariable)]
factToConstraints (AssignLit variable literal _) =
  [TVar (TypeVar variable) :~: literalType literal]
factToConstraints (CmdArg command argumentName variableName _) =
  maybe
    []
    (\resolvedType -> [TVar (TypeVar variableName) :~: resolvedType])
    (lookupArgType command argumentName)
factToConstraints _ = []
