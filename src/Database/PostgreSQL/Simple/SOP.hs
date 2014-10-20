{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, FlexibleInstances, ConstraintKinds, DataKinds, GADTs #-}

module Database.PostgreSQL.Nice.Generic (gfromRow, gtoRow, gselect, ginsertInto) where

import Generics.SOP
import Control.Applicative
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.String (fromString)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

-- https://ocharles.org.uk/blog/posts/2014-08-07-postgresql- simple-generic-sop.html

gfromRow
  :: (All FromField xs, Code a ~ '[xs], SingI xs, Generic a)
  => RowParser a
gfromRow = to . SOP . Z <$> hsequence (hcpure fromFieldp field)
  where fromFieldp = Proxy :: Proxy FromField

gtoRow :: (Generic a, Code a ~ '[xs], All ToField xs, SingI xs) => a -> [Action]
gtoRow a =
  case from a of
    SOP (Z xs) -> hcollapse (hcliftA toFieldP (K . toField . unI) xs)

  where toFieldP = Proxy :: Proxy ToField

fieldNames :: forall a. (Generic a, HasDatatypeInfo a) => Proxy a -> [String]
fieldNames _ = case datatypeInfo (Proxy :: Proxy a) of
  ADT     _ _ cs -> fNms cs
  Newtype _ _ c -> fNms $ c :* Nil

fNms :: NP ConstructorInfo a -> [String]
fNms ((Record _ fs) :* _) = fNmsRec fs

fNmsRec :: NP FieldInfo a -> [String]
fNmsRec Nil = []
fNmsRec (FieldInfo nm :* rest) = nm : fNmsRec rest

-- gselect conn "from persons where name = ? " theName

gselect :: forall r q. (ToRow q, FromRow r, Generic r, HasDatatypeInfo r) => Connection -> Query -> q -> IO [r]
gselect conn q1 args = query conn ("select (" <> (fromString $ intercalate "," $ fieldNames $ (Proxy :: Proxy r) ) <> ") " <> q1) args

ginsertInto :: forall r. (ToRow r, Generic r, HasDatatypeInfo r) => Connection -> Query -> r -> IO ()
ginsertInto conn tbl val = do
  let fnms = fieldNames $ (Proxy :: Proxy r)
  _ <- execute conn ("INSERT INTO " <> tbl <> " (" <> 
                     (fromString $ intercalate "," fnms ) <> 
                     ") VALUES (" <> 
                     (fromString $ intercalate "," $ map (const "?") fnms) <> ")")
               val
  return ()
