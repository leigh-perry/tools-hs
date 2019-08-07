{-# LANGUAGE FlexibleContexts #-}

module KafkaTopologyGen
  ( genTopology
  ) where

import ConfigData (isPk, pkConstraints)
import Data.Char (toLower, toUpper)
import Data.Foldable (traverse_)
import Data.Map.Strict ((!?))
import Ddl ()
import Ddl (pkcColumnName)
import QueryParser
import qualified Shared (distinct)
import qualified Text.Casing as Casing (pascal)

genTopology :: Analysis -> [String] -> IO ()
genTopology a gkts = do
  let gns = globalKtableNames a gkts
  let jps = nonGlobalJoinPoints a gkts True
  let rkjps = nonGlobalJoinPoints a gkts False
  let djps = distinctJoinPointsPlusFrom a
  let djpids = distinctJoinPointIds a
  let serdeTables = Shared.distinct $ jpTable <$> djps
  putStrLn "\n//// Key serdes\n"
  traverse_ printSerdeKey djpids
  putStrLn "\n//// Value serdes\n"
  traverse_ printSerdeValue serdeTables
  putStrLn "\n//// Topic names\n"
  traverse_ printTopicName $ aTables a
  putStrLn "\n//// Global KTables\n"
  traverse_ printGlobalKTable gns
  putStrLn "\n//// KTables using topic key\n"
  traverse_ printKTable jps
  putStrLn "\n//// KTables with re-key\n"
  traverse_ printKTableRekeyed rkjps
  putStrLn "\n//// Principal table stream\n"
  printFromStream $ rName (qFrom $ aQuery a)
  putStrLn "\n//// Accumulated result of the join\n"
  printAccumulationClass a
  putStrLn "\n//////////////// gen-schema.sh ////////////////\n"
  printJoinPointTableKeys a
  putStrLn "\n////////////////\n"
  print $ aResolvedJoins a
  print $ aFromTable a

printSerdeKey :: JoinPoint -> IO ()
printSerdeKey jp = do
  let t = Casing.pascal $ jpTable jp
  let c = Casing.pascal $ jpColumn jp
  putStrLn $ "implicit val hasSerde_" <> t <> "Key" <> c <> ": HasSerde[" <> t <> "Key" <> c <> "] ="
  putStrLn "  HasSerde.instance(cfg => Kafka.avroSerde(cfg.schemaRegistryConfig, isKey = true))"

printSerdeValue :: String -> IO ()
printSerdeValue table = do
  let ts = Casing.pascal table
  putStrLn $ "implicit val hasSerde_" <> ts <> "Envelope: HasSerde[" <> ts <> "Envelope] ="
  putStrLn "  HasSerde.instance(cfg => Kafka.avroSerde(cfg.schemaRegistryConfig, isKey = false))"
  putStrLn $ "implicit val hasSerde_" <> ts <> ": HasSerde[" <> ts <> "] ="
  putStrLn "  HasSerde.instance(cfg => Kafka.avroSerde(cfg.schemaRegistryConfig, isKey = false))"

printTopicName :: String -> IO ()
printTopicName table = do
  let lc = toLower <$> table
  let ts = Casing.pascal table
  putStrLn $ "val cleansed_" <> ts <> " = TopicName(\"cld-prod-sor-pc-" <> lc <> "\")"

printGlobalKTable :: String -> IO ()
printGlobalKTable table = do
  let ts = Casing.pascal table
  let pkc = pkConstraints !? (toUpper <$> table)
  let cs =
        Casing.pascal $
        case pkc of
          Just pk -> pkcColumnName (head pk)
          Nothing -> "wtf"
  putStrLn $ "val gkt" <> ts <> ": GlobalKTable[" <> ts <> "Key" <> cs <> ", " <> ts <> "Envelope] ="
  putStrLn $ "  globalKTable[" <> ts <> "Key" <> cs <> ", " <> ts <> "Envelope]("
  putStrLn "    cfg,"
  putStrLn "    builder,"
  putStrLn $ "    cleansed_" <> ts <> ","
  putStrLn $ "    \"store-" <> ts <> "\""
  putStrLn "  )"

printKTable :: JoinPoint -> IO ()
printKTable jp = do
  let ts = Casing.pascal $ jpTable jp
  let cs = Casing.pascal $ jpColumn jp
  let key = ts <> "Key" <> cs
  putStrLn $ "val kt" <> ts <> "_" <> cs <> ": KTable[" <> key <> ", " <> ts <> "] ="
  putStrLn $ "  kTable[" <> key <> ", " <> ts <> "Envelope, " <> ts <> "]("
  putStrLn "    cfg,"
  putStrLn "    builder,"
  putStrLn $ "    cleansed_" <> ts <> ","
  putStrLn "    _.after,"
  putStrLn $ "    \"store-" <> ts <> "\""
  putStrLn " )"

printKTableRekeyed :: JoinPoint -> IO ()
printKTableRekeyed jp =
  if isOptional
    then printOpt
    else printNonOpt
  where
    t = jpTable jp
    c = jpColumn jp
    ts = Casing.pascal t
    cs = Casing.pascal c
    oldkey = ts <> "Key" <> "Id" -- TODO look up
    newkey = ts <> "Key" <> cs
    newKeyType = "Long" -- TODO determine type
    newkeywrapper = ts <> "_" <> cs
    envtype = ts <> "Envelope"
    isOptional = False {-not $ isPk t c-}
     -- TODO wrong: check nullable
    printNonOpt = do
      putStrLn $ "val kt" <> newkeywrapper <> ": KTable[" <> newkey <> ", " <> ts <> "] ="
      putStrLn $
        "  kTableRekeyed[" <> oldkey <> ", " <> envtype <> ", " <> ts <> ", " <> newKeyType <> ", " <> newkey <> "]("
      putStrLn "    cfg,"
      putStrLn "    builder,"
      putStrLn $ "    cleansed_" <> ts <> ","
      putStrLn "    _.after,"
      putStrLn $ "    _." <> (toUpper <$> cs) <> ","
      putStrLn $ "    " <> newkey <> "(_)"
      putStrLn " )"
    printOpt = do
      putStrLn $ "val kt" <> newkeywrapper <> ": KTable[" <> newkey <> ", " <> ts <> "] ="
      putStrLn $
        "  kTableRekeyedOptional[" <> newkey <> ", " <> envtype <> ", " <> ts <> ", " <> newKeyType <> ", " <>
        newkeywrapper <>
        "]("
      putStrLn "    cfg,"
      putStrLn "    builder,"
      putStrLn $ "    cleansed_" <> ts <> ","
      putStrLn "    _.after,"
      putStrLn $ "    _." <> (toUpper <$> cs) <> ","
      putStrLn $ "    " <> newkey <> "(_)"
      putStrLn " )"

printFromStream :: String -> IO ()
printFromStream table = do
  let ts = Casing.pascal table
  putStrLn $ "val fromStream = kStream[" <> ts <> "Key, " <> ts <> "Envelope](cfg, builder, cleansed_" <> ts <> ")"

printAccumulationClass :: Analysis -> IO ()
printAccumulationClass a = do
  putStrLn "private case class Accumulation("
  putStrLn $ "  streamValue: " <> fts <> "Envelope,"
  traverse_ printJoinResult jps
  putStrLn ")"
  where
    fts = Casing.pascal $ aFromTable a
    jps = distinctJoinPoints a

printJoinResult :: JoinPoint -> IO ()
printJoinResult jp = putStrLn $ "  " <> joinPointName jp <> ": Option[" <> joinPointType jp <> "] = None,"

printJoinPointTableKeys :: Analysis -> IO ()
printJoinPointTableKeys a = traverse_ printJp $ Shared.distinct (distinctJoinPointsPlusFrom a <> distinctJoinPointIds a)
  where
    printJp jp = putStrLn $ "  " <> jpTable jp <> ":" <> jpColumn jp

joinPointName :: JoinPoint -> String
joinPointName jp =
  let ts = Casing.pascal $ jpTable jp
      cs = Casing.pascal $ jpColumn jp
   in "r" <> ts <> "_" <> cs

joinPointType :: JoinPoint -> String
joinPointType jp =
  let ts = Casing.pascal $ jpTable jp
   in ts <> "Envelope"

globalKtableNames :: Analysis -> [String] -> [String]
globalKtableNames a globalKtables = filter (`elem` globalKtables) $ aTables a

nonGlobalJoinPoints :: Analysis -> [String] -> Bool -> [JoinPoint]
nonGlobalJoinPoints a gkts pk = filter f $ distinctJoinPoints a
  where
    f :: JoinPoint -> Bool
    f jp =
      let t = jpTable jp
          c = jpColumn jp
       in notElem t gkts && pk == isPk t c

distinctJoinPoints :: Analysis -> [JoinPoint]
distinctJoinPoints a = Shared.distinct $ (\rj -> [rjFrom rj, rjTo rj]) =<< aResolvedJoins a

distinctJoinPointsPlusFrom :: Analysis -> [JoinPoint]
distinctJoinPointsPlusFrom a = distinctJoinPoints a <> [JoinPoint f fpk]
  where
    f = rName $ qFrom $ aQuery a
    fpk = "id" -- TODO look up

distinctJoinPointIds :: Analysis -> [JoinPoint]
distinctJoinPointIds a = Shared.distinct $ (\rj -> (rjFrom rj) {jpColumn = "id"}) <$> aResolvedJoins a
