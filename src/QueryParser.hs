{-# LANGUAGE FlexibleContexts #-}

module QueryParser where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Either.Combinators (maybeToRight)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (Map, fromList)
import Shared (distinct)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

ws :: Parser String
ws = many (oneOf " ")

noneol :: Parser Char
noneol = noneOf ['\r', '\n']

sym :: Parser String
sym = many1 $ noneOf ['\r', '\n', ' ', '.']

entireLine :: Parser String
entireLine = ws *> many1 noneol <* ws <* endOfLine

joinLine :: String -> (NameAlias -> TableColumn -> TableColumn -> Join) -> Parser Join
joinLine phrase constructor = do
  _ <- ws *> string phrase *> ws
  relationName <- sym <* ws
  alias <- sym <* ws
  _ <- string "ON" *> ws
  srcRelationName <- sym <* string "."
  srcColumnName <- sym <* ws
  _ <- string "=" *> ws
  targetRelationName <- sym <* string "."
  targetColumnName <- sym <* ws
  _ <- endOfLine
  let targetRelation = NameAlias relationName $ Just alias
  let src = TableColumn srcRelationName srcColumnName
  let target = TableColumn targetRelationName targetColumnName
  return $ constructor targetRelation src target

sqlParser :: Parser Query
sqlParser = do
  let wsFrom = ws *> string "FROM" *> ws
  let wsWhere = ws *> string "WHERE" *> ws
  _ <- ws *> string "SELECT" <* ws <* endOfLine
  selectLines <- manyTill entireLine $ try $ lookAhead wsFrom
  relationName <- wsFrom *> sym <* ws
  alias <- sym <* ws <* endOfLine
  joins <-
    manyTill (try (joinLine "JOIN" (Join Inner)) <|> joinLine "LEFT JOIN" (Join LeftOuter)) $ try $ lookAhead wsWhere
  where1 <- wsWhere *> entireLine
  where2 <- many1 entireLine
  eof
  return $ Query selectLines (NameAlias relationName $ Just alias) joins (where1 : where2)

----
data ParseSqlError
  = ParseSqlError ParseError
  | UnknownAlias String
  deriving (Show, Eq)

data NameAlias =
  NameAlias
    { rName :: String
    , rAlias :: Maybe String
    }
  deriving (Show, Eq)

data TableColumn =
  TableColumn
    { tcTableName :: String -- or alias
    , tcColumnName :: String
    }
  deriving (Show, Eq)

data JoinType
  = Inner
  | LeftOuter
  deriving (Show, Eq)

data Join =
  Join
    { jType :: JoinType
    , jTarget :: NameAlias
    , jFrom :: TableColumn
    , jTo :: TableColumn
    }
  deriving (Show, Eq)

data Query =
  Query
    { qSelectLines :: [String]
    , qFrom :: NameAlias
    , qJoins :: [Join]
    , qWhereClause :: [String]
    }
  deriving (Show, Eq)

data JoinPoint =
  JoinPoint
    { jpTable :: String -- alias-resolved table name
    , jpColumn :: String
    }
  deriving (Show, Eq)

data ResolvedJoin =
  ResolvedJoin
    { rjFrom :: JoinPoint
    , rjTo :: JoinPoint
    }
  deriving (Show, Eq)

data Analysis =
  Analysis
    { aQuery :: Query
    , aFromTable :: String
    , aResolvedJoins :: [ResolvedJoin]
    , aTables :: [String]
    , aGlobalKtables :: [String]
    , aDistinctJoinPoints :: [JoinPoint]
    , aKeySerdes :: [JoinPoint]
    , aValueSerdes :: [String]
    }
  deriving (Show, Eq)

----
analyse :: String -> [String] -> ExceptT ParseSqlError IO Analysis
analyse filepath globalKtables = do
  s <- liftIO $ readFile filepath
  let result = parse sqlParser "SQL" s
  except $
    case result of
      Left e -> Left $ ParseSqlError e
      Right query -> resolve query globalKtables

resolve :: Query -> [String] -> Either ParseSqlError Analysis
resolve query globalKtables = do
  let tnm = tableNameMap query
  resolvedJoins <- traverse (resolveJoin tnm) $ qJoins query
  let from = rName $ qFrom query
  let joinTables = rName . jTarget <$> qJoins query
  let allTables = from : joinTables
  let gktables = filter (`elem` globalKtables) allTables
  let distinctJoinPoints = distinct $ (\rj -> [rjFrom rj, rjTo rj]) =<< resolvedJoins
  -- key serde for distinct [ (every joinpoint table)*(id,joincol) + from(id) ]
  let keySerdes =
        distinct $
        [JoinPoint from "id"] <> -- from table
        distinctJoinPoints <> -- join point columns
        ((\jp -> jp {jpColumn = "id"}) <$> distinctJoinPoints) <> -- table primary keys
        ((`JoinPoint` "id") <$> gktables) -- globalktable primary keys
  let valueSerdes = distinct $ from : (jpTable <$> distinctJoinPoints) -- value serde for distinct [ (JoinPoint tables) + from ]
  return $
    Analysis
      { aQuery = query
      , aFromTable = from
      , aResolvedJoins = resolvedJoins
      , aTables = allTables
      , aGlobalKtables = gktables
      , aDistinctJoinPoints = distinctJoinPoints
      , aKeySerdes = keySerdes
      , aValueSerdes = valueSerdes
      }

tableNameMap :: Query -> Map.Map String String
tableNameMap q = Map.fromList $ fromNames <> joinNames
  where
    fromNames = mapAlias $ qFrom q
    joinNames = mapAlias =<< (jTarget <$> qJoins q)
    mapAlias :: NameAlias -> [(String, String)]
    mapAlias n =
      case rAlias n of
        Just a -> [(a, t), (t, t)]
        Nothing -> [(t, t)]
      where
        t = rName n

resolveJoin :: Map.Map String String -> Join -> Either ParseSqlError ResolvedJoin
resolveJoin tnm j = do
  tf <- tableName (tcTableName $ jFrom j)
  tt <- tableName (tcTableName $ jTo j)
  return $ ResolvedJoin (JoinPoint tf (tcColumnName (jFrom j))) (JoinPoint tt (tcColumnName (jTo j)))
  where
    tableName :: String -> Either ParseSqlError String
    tableName t = maybeToRight (UnknownAlias t) (tnm !? t)
{-
  --  let jps = nonGlobalJoinPoints rjs gkts True
  --  let reykeyjps = nonGlobalJoinPoints rjs gkts False
  --  let djpspk = distinctJoinPointPks rjs
  -- let djps = distinctJoinPointsPlusFrom rhs
  -- let djpids =
  -- let serdeTables = distinct $ jpTable <$> djps

-- Bool indicates if must be primary key column
nonGlobalJoinPoints :: [ResolvedJoin] -> [String] -> Bool -> [JoinPoint]
nonGlobalJoinPoints rjs gkts pk = filter f $ distinctJoinPoints rjs
  where
    f :: JoinPoint -> Bool
    f jp =
      let t = jpTable jp
          c = jpColumn jp
       in notElem t gkts && pk == isPk t c

--distinctJoinPointsPlusFrom :: [ResolvedJoin] -> [JoinPoint]
--distinctJoinPointsPlusFrom a = distinctJoinPoints a <> [JoinPoint f fpk]
--  where
--    f = rName $ qFrom $ aQuery a
--    fpk = "id" -- TODO look up
distinctJoinPointPks :: [ResolvedJoin] -> [JoinPoint]
distinctJoinPointPks rjs = distinct $ (\rj -> (rjFrom rj) {jpColumn = "id"}) <$> rjs
-}