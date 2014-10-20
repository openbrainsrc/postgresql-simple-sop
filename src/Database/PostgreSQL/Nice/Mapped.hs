{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, FlexibleInstances, ConstraintKinds, GADTs #-}

module Database.PostgreSQL.Nice.Mapped where

import Generics.SOP
import qualified GHC.Generics as GHC

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
