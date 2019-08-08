{-# LANGUAGE FlexibleContexts #-}

module KafkaTopologyGen
  ( genTopology
  ) where

import ConfigData (pkConstraints)
import Data.Char (toLower, toUpper)
import Data.Foldable (traverse_)
import Data.Map.Strict ((!?))
import Ddl (pkcColumnName)
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
  traverse_ printKTable $ aDistinctJoinPoints a
  putStrLn "\n//// KTables with re-key\n"
  -- traverse_ printKTableRekeyed rkjps
  putStrLn "\n//// Principal table stream\n"
  printFromStream $ rName (qFrom $ aQuery a)
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
  if isOptional
    then printOpt
    else printNonOpt
  where
    t = jpTable jp
    c = jpColumn jp
    ts = sym [t]
    cs = sym [c]
    oldkey = ts <> "id" <> "key" -- TODO look up
    newkey = sym [ts, cs, "key"]
    newKeyType = "Long" -- TODO determine type
    newkeywrapper = ts <> "_" <> cs
    env = ts <> "Envelope"
    isOptional = False {-not $ isPk t c-}
     -- TODO wrong: check nullable
    printNonOpt = do
      putStrLn $ "val kt" <> newkeywrapper <> ": KTable[" <> newkey <> ", " <> ts <> "] ="
      putStrLn $
        "  kTableRekeyed[" <> oldkey <> ", " <> env <> ", " <> ts <> ", " <> newKeyType <> ", " <> newkey <> "]("
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
        "  kTableRekeyedOptional[" <> newkey <> ", " <> env <> ", " <> ts <> ", " <> newKeyType <> ", " <> newkeywrapper <>
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
  let ts = sym [table]
  putStrLn $
    "val fromStream = kStream[" <> ts <> "Key, " <> ts <> "Envelope" <> "](cfg, builder, cleansed_" <> ts <> ")"

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
printJoinResult jp = putStrLn $ "  " <> joinPointName jp <> ": Option[" <> joinPointType jp <> "] = None,"

printForAvscGen :: Analysis -> IO ()
printForAvscGen a = traverse_ printJp $ distinct (aKeySerdes a <> ((`JoinPoint` "id") <$> aValueSerdes a))
  where
    printJp jp = putStrLn $ "  " <> jpTable jp <> ":" <> jpColumn jp
    tablePkJps = (`JoinPoint` "id") <$> aTables a

joinPointName :: JoinPoint -> String
joinPointName jp =
  let ts = sym [jpTable jp]
      cs = sym [jpColumn jp]
   in "r" <> ts <> "_" <> cs

joinPointType :: JoinPoint -> String
joinPointType jp =
  let ts = sym [jpTable jp]
   in ts <> "Envelope"
