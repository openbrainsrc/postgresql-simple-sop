{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, FlexibleInstances, ConstraintKinds, DataKinds, GADTs #-}

module Database.PostgreSQL.Nice.Generic (gfromRow, gtoRow) where

import Generics.SOP
import qualified GHC.Generics as GHC
import Control.Applicative

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

-- https://ocharles.org.uk/blog/posts/2014-08-07-postgresql- simple-generic-sop.html

fields :: (All FromField xs, SingI xs) => RowParser (NP I xs)
fields = hsequence (hcpure fromField field)
  where fromField = Proxy :: Proxy FromField

gfromRow
  :: (All FromField xs, Code a ~ '[xs], SingI xs, Generic a)
  => RowParser a
gfromRow = to . SOP . Z <$> hsequence (hcpure fromField field)
  where fromField = Proxy :: Proxy FromField

gtoRow :: (Generic a, Code a ~ '[xs], All ToField xs, SingI xs) => a -> [Action]
gtoRow a =
  case from a of
    SOP (Z xs) -> hcollapse (hcliftA toFieldP (K . toField . unI) xs)

  where toFieldP = Proxy :: Proxy ToField
