{-# LANGUAGE DefaultSignatures, OverloadedStrings, ScopedTypeVariables, DeriveGeneric, FlexibleInstances, ConstraintKinds, DataKinds, GADTs, TypeOperators, FlexibleContexts #-}

{- |

Generic functions to make working with postgresql-simple easier.

Original implmentation of gfromRow and gtoRow by
<https://ocharles.org.uk/blog/posts/2014-08-07-postgresql-simple-generic-sop.html Ollie Charles>.

Intended usage:

@
import qualified GHC.Generics as GHC
import Generics.SOP

data Person = Person { name:: String, age:: Int } deriving (GHC.Generic)

instance Generic Person
instance HasDatatypeInfo Person

instance FromRow Person where fromRow = gfromRow
instance ToRow Person where toRow = gtoRow
@

-}

module Database.PostgreSQL.Simple.SOP (gfromRow, gtoRow, gselectFrom, ginsertInto, ginsertManyInto, HasFieldNames, fieldNames) where

import Generics.SOP
import Control.Applicative
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.String (fromString)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

--

-- |Generic fromRow
gfromRow
  :: (All FromField xs, Code a ~ '[xs], Generic a)
  => RowParser a
gfromRow = to . SOP . Z <$> hsequence (hcpure fromFieldp field)
  where fromFieldp = Proxy :: Proxy FromField

-- |Generic toRow
gtoRow :: (Generic a, Code a ~ '[xs], All ToField xs) => a -> [Action]
gtoRow a =
  case from a of
    SOP (Z xs) -> hcollapse (hcliftA toFieldP (K . toField . unI) xs)

  where toFieldP = Proxy :: Proxy ToField

fNms :: NP ConstructorInfo a -> [String]
fNms ((Record _ fs) :* _) = fNmsRec fs

fNmsRec :: NP FieldInfo a -> [String]
fNmsRec Nil = []
fNmsRec (FieldInfo nm :* rest) = nm : fNmsRec rest

--

class HasFieldNames a where
  fieldNames :: Proxy a -> [String]

  default fieldNames :: (Generic a, HasDatatypeInfo a) => Proxy a -> [String]
  fieldNames p = case datatypeInfo p of
    ADT     _ _ cs -> fNms cs
    Newtype _ _ c -> fNms $ c :* Nil

instance (HasFieldNames a, HasFieldNames b) => HasFieldNames (a:.b) where
  fieldNames proxy =fieldNames (Proxy::Proxy a) ++ fieldNames (Proxy::Proxy b)

{-|Generic select

@
gselectFrom conn \"persons where name = ?\" theName
@

-}
gselectFrom :: forall r q. (ToRow q, FromRow r, Generic r, HasFieldNames r) => Connection -> Query -> q -> IO [r]
gselectFrom conn q1 args = do
  let fullq = "select " <> (fromString $ intercalate "," $ fieldNames $ (Proxy :: Proxy r) ) <> " from " <> q1
  query conn fullq args

{-|Generic insert

@
let thePerson = Person \"Tom\" 37
ginsertInto conn \"persons\" thePerson
@

This is not going to work if you use auto-incrementing primary keys and the primary key is part of the Haskell record.
-}
ginsertInto :: forall r. (ToRow r, Generic r, HasFieldNames r) => Connection -> Query -> r -> IO ()
ginsertInto conn tbl val = do
  let fnms = fieldNames $ (Proxy :: Proxy r)
  _ <- execute conn ("INSERT INTO " <> tbl <> " (" <>
                     (fromString $ intercalate "," fnms ) <>
                     ") VALUES (" <>
                     (fromString $ intercalate "," $ map (const "?") fnms) <> ")")
               val
  return ()

ginsertManyInto :: forall r. (ToRow r, Generic r, HasFieldNames r) => Connection -> Query -> [r] -> IO ()
ginsertManyInto conn tbl vals = do
  let fnms = fieldNames $ (Proxy :: Proxy r)
  _ <- executeMany conn ("INSERT INTO " <> tbl <> " (" <>
                     (fromString $ intercalate "," fnms ) <>
                     ") VALUES (" <>
                     (fromString $ intercalate "," $ map (const "?") fnms) <> ")")
               vals
  return ()
