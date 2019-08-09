import AvroSchemaGen (genAvsc)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Functor (void)
import Data.Semigroup ((<>))
import Debug.Trace
import KafkaTopologyGen (genTopology)
import Options.Applicative
import QueryParser (aTables, analyse)
import Shared (distinct)

data Command
  = GenerateTopology !FilePath ![String] -- oSqlFilepath, oGlobalKTables
  | GenerateSchema !String !String !FilePath !FilePath !String -- oTableName oKeyColumnName oKeyAvscFilepath oValueAvscFilepath namespace
  | SqlOverview ![String] -- allTables
  deriving (Show, Eq)

commandParser :: ParserInfo Command
commandParser = addInfo programOptions "tools-hs [ gen-topology | gen-schema ]"
  where
    programOptions :: Parser Command
    programOptions =
      (subparser . foldMap expand)
        [ ("gen-topology", genTopologyOpts, "Test-parse the SQL")
        , ("gen-schema", genSchemaOpts, "Generate the Avro schema for a table")
        , ("sql-overview", sqlOverviewOpts, "Create overview for multiple queries")
        ]
    genTopologyOpts = GenerateTopology <$> filepathArg "SQL" <*> globalKTablesArg
    genSchemaOpts =
      GenerateSchema <$> tableNameArg <*> keyColumnArg <*> filepathArg "Avro schema for key" <*>
      filepathArg "Avro schema for value" <*>
      namespaceArg
    sqlOverviewOpts = SqlOverview <$> sqlFilesArg
    filepathArg :: String -> Parser String
    filepathArg s = strArgument (help (s <> " file path") <> metavar "FILEPATH")
    tableNameArg = strArgument (help "Table name" <> metavar "TABLE")
    keyColumnArg = strArgument (help "Key column name" <> metavar "COLUMN")
    globalKTablesArg = read <$> strArgument (help "Global KTables" <> metavar "[GLOBALKTABLES]")
    sqlFilesArg = read <$> strArgument (help "SQL files" <> metavar "[SQLFILES]")
    namespaceArg = strArgument (help "Namespace" <> metavar "NAMESPACE")
    expand :: (String, Parser a, String) -> Mod CommandFields a
    expand (cmdName, parser, desc) = command cmdName $ addInfo parser desc
    addInfo :: Parser a -> String -> ParserInfo a
    addInfo p desc = info (helper <*> p) (fullDesc <> progDesc desc)

main :: IO ()
main = do
  cmd <- execParser commandParser
  case cmd of
    GenerateTopology sqlFp globalKtables ->
      void $
      runExceptT $ do
        a <- QueryParser.analyse globalKtables sqlFp
        liftIO $ void $ KafkaTopologyGen.genTopology a globalKtables
    GenerateSchema tableName keyColumnName keyAvscFp valueAvscFp ns ->
      runExceptT (AvroSchemaGen.genAvsc tableName keyColumnName keyAvscFp valueAvscFp ns) >>= print
    SqlOverview sqls ->
      void $
      runExceptT $ do
        ans <- traverse (QueryParser.analyse []) sqls
        let tables = aTables =<< ans
        liftIO $ print $ distinct tables
