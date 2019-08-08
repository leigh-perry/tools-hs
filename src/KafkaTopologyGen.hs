{-# LANGUAGE FlexibleContexts #-}

module KafkaTopologyGen
  ( genTopology
  ) where

import ConfigData (findColumn, pkConstraints)
import Data.Char (toLower, toUpper)
import Data.Foldable (traverse_)
import Data.Map.Strict ((!?))
import Ddl
import QueryParser hiding (sym)
import Shared (distinct, sym)

genTopology :: Analysis -> [String] -> IO ()
genTopology a gkts = do
  putStrLn "\n//// Key serdes\n"
  traverse_ printSerdeKey $ aKeySerdes a
  putStrLn "\n//// Value serdes\n"
  traverse_ printSerdeValue $ aValueSerdes a
  putStrLn "\n//// Topic names\n"
  traverse_ printTopicName $ aTables a
  putStrLn "\n//// Global KTables\n"
  traverse_ printGlobalKTable $ aGlobalKtables a
  putStrLn "\n//// KTables using topic key\n"
  traverse_ printKTable $ aPkJoinPoints a
  putStrLn "\n//// KTables with re-key\n"
  traverse_ printKTableRekeyed $ aRekeyJoinPoints a
  putStrLn "\n//// Principal table stream\n"
  printFromStream $ naName (qFrom $ aQuery a)
  putStrLn "\n//// Accumulated result of the join\n"
  printAccumulationClass a
  putStrLn "\n//////////////// gen-schema.sh ////////////////\n"
  printForAvscGen a
  putStrLn "\n////////////////\n"
  print $ aResolvedJoins a
  print $ aFromTable a
  print $ aKeySerdes a
  print $ aValueSerdes a

printSerdeKey :: JoinPoint -> IO ()
printSerdeKey jp = do
  let s = sym [jpTable jp, jpColumn jp, "key"]
  putStrLn $ "implicit val hasSerde_" <> s <> ": HasSerde[" <> s <> "] = keySerde[" <> s <> "]"

printSerdeValue :: String -> IO ()
printSerdeValue table = do
  let ts = sym [table]
  let env = ts <> "Envelope"
  putStrLn $ "implicit val hasSerde_" <> env <> ": HasSerde[" <> ts <> "Envelope] =" <> " valueSerde[" <> env <> "]"
  putStrLn $ "implicit val hasSerde_" <> ts <> ": HasSerde[" <> ts <> "] =" <> " valueSerde[" <> ts <> "]"

printTopicName :: String -> IO ()
printTopicName table = do
  let lc = toLower <$> table
  let ts = sym [table]
  putStrLn $ "val cleansed_" <> ts <> " = TopicName(\"cld-prod-sor-pc-" <> lc <> "\")"

printGlobalKTable :: String -> IO ()
printGlobalKTable t = do
  let pkc = pkConstraints !? (toUpper <$> t)
  let cs =
        sym
          [ case pkc of
              Just pk -> pkcColumnName (head pk)
              Nothing -> "wtf" -- TODO handle impossible error?
          ]
  let key = sym [t, cs, "key"]
  let env = sym [t, "Envelope"]
  let ts = sym [t]
  putStrLn $ "val gkt" <> t <> ": GlobalKTable[" <> key <> ", " <> env <> "] ="
  putStrLn $ "  globalKTable[" <> key <> ", " <> env <> "]("
  putStrLn "    cfg,"
  putStrLn "    builder,"
  putStrLn $ "    cleansed_" <> ts <> ","
  putStrLn $ "    \"store-" <> ts <> "\""
  putStrLn "  )"

printKTable :: JoinPoint -> IO ()
printKTable jp = do
  let t = jpTable jp
  let c = jpColumn jp
  let ts = sym [t]
  let cs = sym [c]
  let key = sym [t, c, "key"]
  let env = sym [t, "Envelope"]
  putStrLn $ "val kt" <> key <> ": KTable[" <> key <> ", " <> ts <> "] ="
  putStrLn $ "  kTable[" <> key <> ", " <> env <> ", " <> ts <> "]("
  putStrLn "    cfg,"
  putStrLn "    builder,"
  putStrLn $ "    cleansed_" <> ts <> ","
  putStrLn "    _.after,"
  putStrLn $ "    \"store-" <> ts <> "\""
  putStrLn " )"

printKTableRekeyed :: JoinPoint -> IO ()
printKTableRekeyed jp =
  if opt
    then printOpt
    else printNonOpt
  where
    t = jpTable jp
    c = jpColumn jp
    ts = sym [t]
    cs = sym [c]
    idKey = sym [t, "id", "key"] -- TODO look up pk name
    newkey = sym [t, cs, "key"]
    column = findColumn t c
    newKeyType = typeOf $ column
    env = sym [t, "Envelope"]
    opt = cNullability column == Nullable
    printNonOpt = do
      putStrLn $ "val kt" <> newkey <> ": KTable[" <> newkey <> ", " <> ts <> "] ="
      putStrLn $
        "  kTableRekeyed[" <> idKey <> ", " <> env <> ", " <> ts <> ", " <> newKeyType <> ", " <> newkey <> "]("
      putStrLn "    cfg,"
      putStrLn "    builder,"
      putStrLn $ "    cleansed_" <> ts <> ","
      putStrLn "    _.after,"
      putStrLn $ "    _." <> (toUpper <$> cs) <> ","
      putStrLn $ "    " <> newkey <> "(_)"
      putStrLn " )"
    printOpt = do
      putStrLn $ "val kt" <> newkey <> ": KTable[" <> newkey <> ", " <> ts <> "] ="
      putStrLn $
        "  kTableRekeyedOptional[" <> newkey <> ", " <> env <> ", " <> ts <> ", " <> newKeyType <> ", " <> newkey <>
        "]("
      putStrLn "    cfg,"
      putStrLn "    builder,"
      putStrLn $ "    cleansed_" <> ts <> ","
      putStrLn "    _.after,"
      putStrLn $ "    _." <> (toUpper <$> cs) <> ","
      putStrLn $ "    " <> newkey <> "(_)"
      putStrLn " )"

printFromStream :: String -> IO ()
printFromStream t = do
  let ts = sym [t]
  let key = sym [t, "id", "key"]
  let env = sym [t, "Envelope"]
  -- putStrLn $ "val fromStream = kStream[" <> key <> "Key, " <> env <> "](cfg, builder, cleansed_" <> ts <> ")"
  putStrLn "val fromStream ="
  putStrLn $ "  kStream[" <> key <> ", " <> env <> ", " <> ts <> "]("
  putStrLn "    cfg,"
  putStrLn "    builder,"
  putStrLn $ "    cleansed_" <> ts <> ","
  putStrLn "    _.after"
  putStrLn "  )"

printAccumulationClass :: Analysis -> IO ()
printAccumulationClass a = do
  putStrLn "private case class Accumulation("
  putStrLn $ "  streamValue: " <> fts <> "Envelope,"
  traverse_ printJoinResult jps
  putStrLn ")"
  where
    fts = sym [aFromTable a]
    jps = aDistinctJoinPoints a

printJoinResult :: JoinPoint -> IO ()
printJoinResult jp = putStrLn $ "  " <> joinPointName <> ": Option[" <> env <> "] = None,"
  where
    env = sym [jpTable jp, "Envelope"]
    joinPointName = "r" <> sym [jpTable jp] <> "_" <> sym [jpColumn jp]

printForAvscGen :: Analysis -> IO ()
printForAvscGen a = traverse_ printJp $ distinct (aKeySerdes a <> ((`JoinPoint` "id") <$> aValueSerdes a))
  where
    printJp jp = putStrLn $ "  " <> jpTable jp <> ":" <> jpColumn jp

typeOf :: Column -> String
typeOf c =
  case toLower <$> cType c of
    "number" -> "Long"
    "varchar2" -> "String"
    "char" -> "String"
    "clob" -> "String"
    "timestamp(6)" -> "Long"
    _ -> error $ cType c -- TODO remove "error" calls
