{-# LANGUAGE FlexibleContexts #-}

module AvroSchemaGen
  ( genAvsc
  ) where

import ConfigData (relations)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Map.Strict ((!?))
import Ddl
import Shared

data GenAvscError
  = UnknownTable String
  | MissingPrimaryKey String
  | UnsupportedColumnType String
  deriving (Show, Eq)

genAvsc :: String -> String -> FilePath -> String -> String -> ExceptT GenAvscError IO ()
genAvsc tableName keyColumnName keyAvscFp valueAvscFp ns = do
  relation <- except (findRelation tableName)
  writeKeyAvsc keyAvscFp ns relation keyColumnName
  writeValueAvsc valueAvscFp ns relation

writeKeyAvsc :: FilePath -> String -> Relation -> String -> ExceptT GenAvscError IO ()
writeKeyAvsc fp ns relation keyColumnName =
  except (relationKeyAvsc relation keyColumnName ns) >>= (liftIO . writeFile fp)

writeValueAvsc :: FilePath -> String -> Relation -> ExceptT GenAvscError IO ()
writeValueAvsc fp ns relation = except (relationValueAvsc relation ns) >>= (liftIO . writeFile fp)

-- constraints <- findPkConstraints (rName r)
-- let pkColumnNames = pkcColumnName <$> constraints
-- let pkColumns = filter (\c -> cName c `elem` pkColumnNames) $ rColumns r
relationKeyAvsc :: Relation -> String -> String -> Either GenAvscError String
relationKeyAvsc r k ns = do
  let ku = toUpper <$> k
  let keyname = sym [rName r, k, "key"]
  let keyColumns = filter (\c -> cName c == ku) $ rColumns r
  c2f <- traverse (columnToField' NotNullable) keyColumns
  let fields = intercalate "," c2f
  Right $ avscPreamble keyname ns <> ",\"fields\": [" <> fields <> "]}"

avscPreamble :: String -> String -> String
avscPreamble name ns = "{\"type\":\"record\",\"name\":\"" <> name <> "\",\"namespace\":\"" <> ns <> "\""

relationValueAvsc :: Relation -> String -> Either GenAvscError String
relationValueAvsc r ns = do
  c2f <- traverse columnToField (rColumns r)
  let fields = intercalate "," c2f
  let name = sym [rName r]
  Right $
    avscPreamble (sym [rName r, "Envelope"]) ns <> "," <> "\"fields\":[" <> "{" <> "\"name\":\"before\"," <>
    "\"type\":[" <>
    "\"null\"," <>
    "{" <>
    "\"type\":\"record\"," <>
    "\"name\":\"" <>
    name <>
    "\"," <>
    "\"fields\":[" <>
    fields <>
    "]" <>
    "}" <>
    "]," <>
    "\"default\":null" <>
    "}," <>
    "{" <>
    "\"name\":\"after\"," <>
    "\"type\":[" <>
    "\"null\"," <>
    "\"" <>
    name <>
    "\"" <>
    "]," <>
    "\"default\":null" <>
    "}" <>
    "]" <>
    "}"

columnToField :: Column -> Either GenAvscError String
columnToField c = columnToField' (cNullability c) c

--columnToType :: Column -> Either GenAvscError String
--columnToType c = columnToType' (cNullability c) c
columnToField' :: Nullability -> Column -> Either GenAvscError String
columnToField' n c = (\s -> "{\"name\": \"" <> cName c <> "\",\"type\": " <> s <> "}") <$> columnToType' n c

columnToType' :: Nullability -> Column -> Either GenAvscError String
columnToType' n c =
  case n of
    NotNullable -> flatten <$> typeOf c
    Nullable -> (\as -> "[\"null\"," <> flatten as <> "],\"default\":null") <$> typeOf c
  where
    flatten :: [Attr] -> String
    flatten as = intercalate "," $ render <$> as
    render :: Attr -> String
    render a =
      case a of
        Kv n v -> show n <> ":" <> v -- show of a string wraps in \"
        Val v -> show v
        Object as -> "{" <> flatten as <> "}"

data Attr
  = Kv String String
  | Val String
  | Object [Attr]

typeOf :: Column -> Either GenAvscError [Attr]
typeOf c =
  case toLower <$> cType c of
    "number" -> Right [Val "long"]
    "varchar2" -> Right [Val "string"]
    "char" -> Right [Val "string"]
    "clob" -> Right [Val "string"]
    "timestamp(6)" ->
      Right
        [ Object
            [ Kv "type" $ show "long"
            , Kv "connect.version" "1"
            , Kv "connect.name" $ show "org.apache.kafka.connect.data.Timestamp"
            ]
        ]
    _ -> Left $ UnsupportedColumnType $ cType c

findRelation :: String -> Either GenAvscError Relation
findRelation tableName =
  case relations !? (toUpper <$> tableName) of
    Just r -> Right r
    _ -> Left $ UnknownTable tableName
{-
findPkConstraints :: String -> Either GenAvscError [PkConstraint]
findPkConstraints tableName =
  case pkConstraints !? (toUpper <$> tableName) of
    Just r -> Right r
    _      -> Left $ MissingPrimaryKey tableName
-}
