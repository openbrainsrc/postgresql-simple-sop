{-# LANGUAGE DefaultSignatures, OverloadedStrings, ScopedTypeVariables, DeriveGeneric, FlexibleInstances, ConstraintKinds, DataKinds, GADTs, TypeOperators, FlexibleContexts, TypeFamilies #-}

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

module Database.PostgreSQL.Simple.SOP (gfromRow, gtoRow, gselectFrom, ginsertInto, ginsertManyInto, HasFieldNames (..), HasTable (..), gselect, ginsertNoKey, HasKey (..), getByKey, gdelete, ginsert, gupdate, gupsert, gfastInsert) where

import Generics.SOP
import Control.Applicative
import Data.Monoid ((<>))
import Data.List (intercalate, intersperse)
import Data.String (fromString)
import Data.Maybe (listToMaybe)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

--

-- |Generic fromRow
gfromRow
  :: (All FromField xs, Code a ~ '[xs], SingI xs, Generic a)
  => RowParser a
gfromRow = to . SOP . Z <$> hsequence (hcpure fromFieldp field)
  where fromFieldp = Proxy :: Proxy FromField

-- |Generic toRow
gtoRow :: (Generic a, Code a ~ '[xs], All ToField xs, SingI xs) => a -> [Action]
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

class (HasFieldNames a, FromRow a, ToRow a) => HasTable a where
  tableName :: Proxy a -> Query

{-|Generic select using table name from class

@
do p :: [Person] <- gselect conn \"where name = ?\" (Only theName)
@

-}
gselect :: forall r q. (ToRow q, FromRow r, HasTable r) => Connection -> Query -> q -> IO [r]
gselect conn q1 args = do
  let fullq = "select " <> (fromString $ intercalate "," $ fieldNames $ (Proxy :: Proxy r) ) <> " from "
                        <> tableName (Proxy :: Proxy r) <> " " <> q1
  query conn fullq args

-- |Insert without knowing about primary keys. Do not use this if your table has a key
ginsertNoKey :: forall r. (ToRow r, HasTable r) => Connection  -> r -> IO ()
ginsertNoKey conn  val = do
  let fnms = fieldNames $ (Proxy :: Proxy r)
  _ <- execute conn ("INSERT INTO " <> tableName (Proxy :: Proxy r) <> " (" <>
                     (fromString $ intercalate "," fnms ) <>
                     ") VALUES (" <>
                     (fromString $ intercalate "," $ map (const "?") fnms) <> ")")
               val
  return ()

class HasTable a => HasKey a where
   type Key a
   getKey :: a -> Key a
   keyName :: Proxy a -> Query
   autoIncrementingKey :: Proxy a -> Bool

-- |Fetch a row by its primary key

getByKey :: forall a . (HasKey a, ToField (Key a)) => Connection -> Key a -> IO (Maybe a)
getByKey conn key = fmap listToMaybe $ gselect conn ("where "<>keyName (Proxy :: Proxy a)<>" = ?") (Only  key)

-- |Delete a row (based on primary key)
gdelete :: forall a . (HasKey a, ToField (Key a)) => Connection -> a -> IO ()
gdelete conn x = do execute conn ("delete from "<>tableName (Proxy :: Proxy a)<>" where "
                                  <> (keyName (Proxy :: Proxy a)<> " = ?")) (Only $ getKey x)
                    return ()

-- |Insert a new value, respecting primary keys whether they are autoincrementing or not
ginsert :: forall a . (HasKey a, ToField (Key a), FromField (Key a)) => Connection -> a -> IO (Key a)
ginsert conn val =
    if autoIncrementingKey proxy 
       then ginsertSerial
    else do 
      ginsertNoKey conn val
      return $ getKey val
   where 
     proxy = Proxy :: Proxy a
     ginsertSerial = do
       let kName = keyName proxy 
           tblName = tableName proxy  
           fldNms = map fromString $ fieldNames proxy 
           fldNmsNoKey = filter (/=kName) fldNms
           qmarks = mconcat $ intersperse "," $ map (const "?") fldNmsNoKey
           qFldNms = mconcat $ intersperse "," fldNmsNoKey
           qArgs = map snd . filter ((/=kName) . fst) . zip fldNms $ toRow val
           q = "insert into "<>tblName<>" ("<>qFldNms<>") values ("<>qmarks<>") returning "<>kName
       res <- query conn q qArgs
       case res of
         [] -> fail $ "no key returned from "++show tblName
         Only k : _ -> return k

-- |Update a row, based on its primary key
gupdate :: forall a . (HasKey a, ToField (Key a)) => Connection -> a -> IO ()
gupdate conn val = do
  let kName = keyName (Proxy :: Proxy a)
      tblName = tableName (Proxy :: Proxy a)
      fldNms = map fromString $ fieldNames (Proxy :: Proxy a)
      fldNmsNoKey = filter (/=kName) fldNms
      rows = toRow val
      vargs = filter ((/=kName) . fst) $ zip fldNms rows
      setq = mconcat $ intersperse "," $ map (\(k,v) -> k <> " = ? " ) vargs
      q = "update "<>tblName<>" set "<>setq<>" where "<>kName<>" = ?"
  execute conn q (map snd vargs ++ [toField $ getKey val])
  return ()

-- |If a row does not exist, insert it; otherwise update it
gupsert :: forall a . (HasKey a, ToField (Key a), FromField (Key a)) => Connection -> a -> IO (Key a)
gupsert conn val = do
  ex :: Maybe a <- getByKey conn $ getKey val
  case ex of
    Nothing -> ginsert conn val
    Just _ -> gupdate conn val >> return (getKey val)

-- |If a row does not exist, insert it; otherwise do nothing; in one SQL query
gfastInsert ::  forall a . (HasKey a, ToField (Key a), FromField (Key a)) => Connection -> a -> IO ()
gfastInsert conn val = do
  let kName = keyName (Proxy :: Proxy a)
      tblName = tableName (Proxy :: Proxy a)
      fldNms = map fromString $ fieldNames (Proxy :: Proxy a)
      rows = toRow val
      vargs = if autoIncrementingKey (Proxy :: Proxy a)
                       then filter ((/=kName) . fst) $ zip fldNms rows
                       else zip fldNms rows
      qmarks = mconcat $ intersperse "," $ map (const "?") vargs
      fields = mconcat $ intersperse "," $ map fst vargs
      q = "insert into "<>tblName<>"("<>fields<>") select "<>qmarks<>
          " where not exists (select 1 from "<>tblName<>" where "<>kName<>"=?)"
      args = map snd vargs
  execute conn q (args :. Only (getKey val))
  return ()
