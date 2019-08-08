module ConfigData
  ( relationsPkConstraints
  , relationsInputs
  , relations
  , pkConstraints
  , isPk
  , findColumn
  ) where

import Data.Char (toUpper)
import Data.List (find, groupBy)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (Map, fromList)
import Ddl

-- Transform raw input config to a lookup table
relations :: Map.Map String Relation
relations = Map.fromList $ (\r -> (rName r, r)) <$> grouped
  where
    grouped = extract <$> group
    group = groupBy (\x y -> riName x == riName y) relationsInputs
    extract :: [Input] -> Relation
    extract cs = Relation {rName = riName $ head cs, rColumns = riColumn <$> cs, rPk = Nothing}

-- Transform raw primary key constraints config to a lookup table
pkConstraints :: Map.Map String [PkConstraint]
pkConstraints = Map.fromList grouped
  where
    grouped = extract <$> group
    group = groupBy (\x y -> pkcTableName x == pkcTableName y) relationsPkConstraints
    extract :: [PkConstraint] -> (String, [PkConstraint])
    extract cs = (pkcTableName (head cs), cs)

isPk :: String -> String -> Bool
isPk tl cl =
  case pkc of
    Just pk -> pkcColumnName (head pk) == c
    Nothing -> False
  where
    t = toUpper <$> tl
    c = toUpper <$> cl
    pkc = pkConstraints !? t

findColumn :: String -> String -> Column
findColumn tl cl = column
  where
    tu = toUpper <$> tl
    cu = toUpper <$> cl
    result = do
      table <- relations !? tu
      find (\c -> cu == cName c) (rColumns table)
    column =
      case result of
        (Just c) -> c
        Nothing -> error "wtf"

-- TODO query DB directly
{-
SELECT
  '  PkConstraint "' ||
  c.table_name || '" "' ||
  c.column_name || '" "' ||
  c.constraint_name || '",'
FROM all_cons_columns c
     INNER JOIN all_constraints d
                ON c.owner = d.owner AND c.table_name = d.table_name AND d.constraint_type = 'P'
                    AND c.constraint_name = d.constraint_name
WHERE
  LOWER(d.owner) = LOWER('pcuser') AND
  LOWER(d.table_name) IN ('pc_personalautocov', 'pc_policyperiod', 'pc_policy', 'pc_job', 'pc_policyline',
                          'pc_uwcompany', 'pctl_policyperiodstatus', 'pc_etlclausepattern', 'pctl_job',
                          'pctl_policyline', 'pctl_currency')
ORDER BY c.table_name, c.column_name;
-}
relationsPkConstraints :: [PkConstraint]
relationsPkConstraints =
  [ PkConstraint "PCTL_CURRENCY" "ID" "PKTL_CURRENCY"
  , PkConstraint "PCTL_JOB" "ID" "PKTL_JOB"
  , PkConstraint "PCTL_POLICYLINE" "ID" "PKTL_POLICYLINE"
  , PkConstraint "PCTL_POLICYPERIODSTATUS" "ID" "PKTL_POLICYPERIODSTATUS"
  , PkConstraint "PC_ETLCLAUSEPATTERN" "ID" "PK_ETLCLAUSEPATTERN"
  , PkConstraint "PC_JOB" "ID" "PK_JOB"
  , PkConstraint "PC_PERSONALAUTOCOV" "ID" "PK_PERSONALAUTOCOV"
  , PkConstraint "PC_POLICY" "ID" "PK_POLICY"
  , PkConstraint "PC_POLICYLINE" "ID" "PK_POLICYLINE"
  , PkConstraint "PC_POLICYPERIOD" "ID" "PK_POLICYPERIOD"
  , PkConstraint "PC_UWCOMPANY" "ID" "PK_UWCOMPANY"
  ]

{-
SELECT
  '  Input "' ||
    t.table_name || '" (Column "' ||
    c.column_name || '" "' ||
    c.data_type || '" ' ||
    CASE WHEN c.data_length IS NULL
           THEN 'Nothing'
         ELSE '(Just ' || c.data_length || ')' END || ' ' ||
    CASE WHEN c.data_precision IS NULL
           THEN 'Nothing'
         ELSE '(Just ' || c.data_precision || ')' END || ' ' ||
    CASE WHEN c.data_scale IS NULL
           THEN 'Nothing'
         ELSE '(Just ' || c.data_scale || ')' END || ' ' ||
    CASE nullable
      WHEN 'N'
        THEN 'NotNullable'
      ELSE
        'Nullable'
      END || ' ' ||
    c.column_id ||
    '),'
FROM sys.all_tables t
     INNER JOIN sys.all_tab_cols c
                ON c.table_name = t.table_name
WHERE LOWER(c.owner) = LOWER('pcuser') AND
      LOWER(t.table_name) IN ('pc_personalautocov', 'pc_policyperiod', 'pc_policy', 'pc_job', 'pc_policyline',
                              'pc_uwcompany', 'pctl_policyperiodstatus', 'pc_etlclausepattern', 'pctl_job',
                              'pctl_policyline', 'pctl_currency')
ORDER BY t.table_name, c.column_id;
-}
relationsInputs :: [Input]
relationsInputs =
  [ Input "PCTL_CURRENCY" (Column "L_EN_US" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 1)
  , Input "PCTL_CURRENCY" (Column "PRIORITY" "NUMBER" (Just 22) Nothing Nothing Nullable 2)
  , Input "PCTL_CURRENCY" (Column "TYPECODE" "VARCHAR2" (Just 200) Nothing Nothing NotNullable 3)
  , Input "PCTL_CURRENCY" (Column "S_EN_US" "NUMBER" (Just 22) Nothing Nothing NotNullable 4)
  , Input "PCTL_CURRENCY" (Column "RETIRED" "CHAR" (Just 4) Nothing Nothing NotNullable 5)
  , Input "PCTL_CURRENCY" (Column "NAME" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 6)
  , Input "PCTL_CURRENCY" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 7)
  , Input "PCTL_CURRENCY" (Column "DESCRIPTION" "VARCHAR2" (Just 2048) Nothing Nothing Nullable 8)
  , Input "PCTL_JOB" (Column "L_EN_US" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 1)
  , Input "PCTL_JOB" (Column "PRIORITY" "NUMBER" (Just 22) Nothing Nothing Nullable 2)
  , Input "PCTL_JOB" (Column "TYPECODE" "VARCHAR2" (Just 200) Nothing Nothing NotNullable 3)
  , Input "PCTL_JOB" (Column "S_EN_US" "NUMBER" (Just 22) Nothing Nothing NotNullable 4)
  , Input "PCTL_JOB" (Column "RETIRED" "CHAR" (Just 4) Nothing Nothing NotNullable 5)
  , Input "PCTL_JOB" (Column "NAME" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 6)
  , Input "PCTL_JOB" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 7)
  , Input "PCTL_JOB" (Column "DESCRIPTION" "VARCHAR2" (Just 2048) Nothing Nothing Nullable 8)
  , Input "PCTL_POLICYLINE" (Column "L_EN_US" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 1)
  , Input "PCTL_POLICYLINE" (Column "PRIORITY" "NUMBER" (Just 22) Nothing Nothing Nullable 2)
  , Input "PCTL_POLICYLINE" (Column "TYPECODE" "VARCHAR2" (Just 200) Nothing Nothing NotNullable 3)
  , Input "PCTL_POLICYLINE" (Column "S_EN_US" "NUMBER" (Just 22) Nothing Nothing NotNullable 4)
  , Input "PCTL_POLICYLINE" (Column "RETIRED" "CHAR" (Just 4) Nothing Nothing NotNullable 5)
  , Input "PCTL_POLICYLINE" (Column "NAME" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 6)
  , Input "PCTL_POLICYLINE" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 7)
  , Input "PCTL_POLICYLINE" (Column "DESCRIPTION" "VARCHAR2" (Just 2048) Nothing Nothing Nullable 8)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "L_EN_US" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 1)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "PRIORITY" "NUMBER" (Just 22) Nothing Nothing Nullable 2)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "TYPECODE" "VARCHAR2" (Just 200) Nothing Nothing NotNullable 3)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "S_EN_US" "NUMBER" (Just 22) Nothing Nothing NotNullable 4)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "RETIRED" "CHAR" (Just 4) Nothing Nothing NotNullable 5)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "NAME" "VARCHAR2" (Just 1024) Nothing Nothing NotNullable 6)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 7)
  , Input "PCTL_POLICYPERIODSTATUS" (Column "DESCRIPTION" "VARCHAR2" (Just 2048) Nothing Nothing Nullable 8)
  , Input "PC_ETLCLAUSEPATTERN" (Column "PUBLICID" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 1)
  , Input "PC_ETLCLAUSEPATTERN" (Column "COVERAGECATEGORY" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 2)
  , Input "PC_ETLCLAUSEPATTERN" (Column "BEANVERSION" "NUMBER" (Just 22) Nothing Nothing Nullable 3)
  , Input "PC_ETLCLAUSEPATTERN" (Column "CLAUSETYPE" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 4)
  , Input "PC_ETLCLAUSEPATTERN" (Column "COVEREDPARTYTYPE" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 5)
  , Input "PC_ETLCLAUSEPATTERN" (Column "NAME" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 6)
  , Input "PC_ETLCLAUSEPATTERN" (Column "PATTERNID" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 7)
  , Input "PC_ETLCLAUSEPATTERN" (Column "COVERAGESUBTYPE" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 8)
  , Input "PC_ETLCLAUSEPATTERN" (Column "CODEIDENTIFIER" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 9)
  , Input "PC_ETLCLAUSEPATTERN" (Column "OWNINGENTITYTYPE" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 10)
  , Input "PC_ETLCLAUSEPATTERN" (Column "SUBTYPE" "NUMBER" (Just 22) Nothing Nothing NotNullable 11)
  , Input "PC_ETLCLAUSEPATTERN" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 12)
  , Input "PC_JOB" (Column "JOBNUMBERDENORM" "VARCHAR2" (Just 240) Nothing Nothing NotNullable 1)
  , Input "PC_JOB" (Column "NOTTAKENNOTIFDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 2)
  , Input "PC_JOB" (Column "ARCHIVESTATE" "NUMBER" (Just 22) Nothing Nothing Nullable 3)
  , Input "PC_JOB" (Column "ARCHIVESCHEMAINFO" "NUMBER" (Just 22) Nothing Nothing Nullable 4)
  , Input "PC_JOB" (Column "UPDATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 5)
  , Input "PC_JOB" (Column "NOTIFICATIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 6)
  , Input "PC_JOB" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 7)
  , Input "PC_JOB" (Column "SOURCE" "NUMBER" (Just 22) Nothing Nothing Nullable 8)
  , Input "PC_JOB" (Column "EXCLUDEREASON" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 9)
  , Input "PC_JOB" (Column "NEXTPURGECHECKDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 10)
  , Input "PC_JOB" (Column "POLICYTERM" "NUMBER" (Just 22) Nothing Nothing NotNullable 11)
  , Input "PC_JOB" (Column "CREATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 12)
  , Input "PC_JOB" (Column "ARCHIVEFAILUREID" "NUMBER" (Just 22) Nothing Nothing Nullable 13)
  , Input "PC_JOB" (Column "REJECTREASON" "NUMBER" (Just 22) Nothing Nothing Nullable 14)
  , Input "PC_JOB" (Column "CLOSEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 15)
  , Input "PC_JOB" (Column "BEANVERSION" "NUMBER" (Just 22) Nothing Nothing Nullable 16)
  , Input "PC_JOB" (Column "RETIRED" "NUMBER" (Just 22) Nothing Nothing NotNullable 17)
  , Input "PC_JOB" (Column "CANCELREASONCODE" "NUMBER" (Just 22) Nothing Nothing Nullable 18)
  , Input "PC_JOB" (Column "CHANGEPOLICYNUMBER" "CHAR" (Just 4) Nothing Nothing Nullable 19)
  , Input "PC_JOB" (Column "UPDATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 20)
  , Input "PC_JOB" (Column "PRIMARYINSUREDNAMEDENORM" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 21)
  , Input "PC_JOB" (Column "NONRENEWALNOTIFDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 22)
  , Input "PC_JOB" (Column "PRIMARYINSUREDNAME" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 23)
  , Input "PC_JOB" (Column "QUOTETYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 24)
  , Input "PC_JOB" (Column "DATEQUOTENEEDED" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 25)
  , Input "PC_JOB" (Column "PUBLICID" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 26)
  , Input "PC_JOB" (Column "SIDEBYSIDE" "CHAR" (Just 4) Nothing Nothing NotNullable 27)
  , Input "PC_JOB" (Column "JOBNUMBER" "VARCHAR2" (Just 240) Nothing Nothing NotNullable 28)
  , Input "PC_JOB" (Column "REWRITETYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 29)
  , Input "PC_JOB" (Column "CREATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 30)
  , Input "PC_JOB" (Column "AUDITINFORMATIONID" "NUMBER" (Just 22) Nothing Nothing Nullable 31)
  , Input "PC_JOB" (Column "POLICYID" "NUMBER" (Just 22) Nothing Nothing NotNullable 32)
  , Input "PC_JOB" (Column "EXCLUDEDFROMARCHIVE" "CHAR" (Just 4) Nothing Nothing Nullable 33)
  , Input "PC_JOB" (Column "REJECTREASONTEXT" "CLOB" (Just 4000) Nothing Nothing Nullable 34)
  , Input "PC_JOB" (Column "ARCHIVEFAILUREDETAILSID" "NUMBER" (Just 22) Nothing Nothing Nullable 35)
  , Input "PC_JOB" (Column "RESCINDNOTIFICATIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 36)
  , Input "PC_JOB" (Column "FROZENSETID" "NUMBER" (Just 22) Nothing Nothing Nullable 37)
  , Input "PC_JOB" (Column "PURGESTATUS" "NUMBER" (Just 22) Nothing Nothing NotNullable 38)
  , Input "PC_JOB" (Column "INITIALNOTIFICATIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 39)
  , Input "PC_JOB" (Column "LASTNOTIFIEDCANCELLATIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 40)
  , Input "PC_JOB" (Column "JOBGROUP" "NUMBER" (Just 22) Nothing Nothing Nullable 41)
  , Input "PC_JOB" (Column "CANCELPROCESSDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 42)
  , Input "PC_JOB" (Column "RENEWALCODE" "NUMBER" (Just 22) Nothing Nothing Nullable 43)
  , Input "PC_JOB" (Column "ESCALATEAFTERHOLDRELEASED" "CHAR" (Just 4) Nothing Nothing Nullable 44)
  , Input "PC_JOB" (Column "RENEWALNOTIFDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 45)
  , Input "PC_JOB" (Column "REINSTATECODE" "NUMBER" (Just 22) Nothing Nothing Nullable 46)
  , Input "PC_JOB" (Column "PAYMENTRECEIVED" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 47)
  , Input "PC_JOB" (Column "QUOTEONSTART" "CHAR" (Just 4) Nothing Nothing Nullable 48)
  , Input "PC_JOB" (Column "PAYMENTRECEIVED_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 49)
  , Input "PC_JOB" (Column "ARCHIVEPARTITION" "NUMBER" (Just 22) Nothing Nothing Nullable 50)
  , Input "PC_JOB" (Column "NOTIFICATIONACKDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 51)
  , Input "PC_JOB" (Column "ARCHIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 52)
  , Input "PC_JOB" (Column "BINDOPTION" "NUMBER" (Just 22) Nothing Nothing Nullable 53)
  , Input "PC_JOB" (Column "NONRENEWALCODE" "NUMBER" (Just 22) Nothing Nothing Nullable 54)
  , Input "PC_JOB" (Column "SUBTYPE" "NUMBER" (Just 22) Nothing Nothing NotNullable 55)
  , Input "PC_JOB" (Column "SUBMISSIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 56)
  , Input "PC_JOB" (Column "DESCRIPTION" "VARCHAR2" (Just 4000) Nothing Nothing Nullable 57)
  , Input "PC_PERSONALAUTOCOV" (Column "PUBLICID" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 1)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM1" "VARCHAR2" (Just 256) Nothing Nothing Nullable 2)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM2" "VARCHAR2" (Just 256) Nothing Nothing Nullable 3)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM3" "VARCHAR2" (Just 256) Nothing Nothing Nullable 4)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM4" "VARCHAR2" (Just 256) Nothing Nothing Nullable 5)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM5" "VARCHAR2" (Just 256) Nothing Nothing Nullable 6)
  , Input "PC_PERSONALAUTOCOV" (Column "CREATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 7)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM6" "VARCHAR2" (Just 256) Nothing Nothing Nullable 8)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM7" "VARCHAR2" (Just 256) Nothing Nothing Nullable 9)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM8" "VARCHAR2" (Just 256) Nothing Nothing Nullable 10)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM7AVL" "CHAR" (Just 4) Nothing Nothing Nullable 11)
  , Input "PC_PERSONALAUTOCOV" (Column "FIXEDID" "NUMBER" (Just 22) Nothing Nothing NotNullable 12)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM5AVL" "CHAR" (Just 4) Nothing Nothing Nullable 13)
  , Input "PC_PERSONALAUTOCOV" (Column "CURRENCY" "NUMBER" (Just 22) Nothing Nothing NotNullable 14)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM3AVL" "CHAR" (Just 4) Nothing Nothing Nullable 15)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM1AVL" "CHAR" (Just 4) Nothing Nothing Nullable 16)
  , Input "PC_PERSONALAUTOCOV" (Column "EFFECTIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 17)
  , Input "PC_PERSONALAUTOCOV" (Column "UPDATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 18)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM3AVL" "CHAR" (Just 4) Nothing Nothing Nullable 19)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM1AVL" "CHAR" (Just 4) Nothing Nothing Nullable 20)
  , Input "PC_PERSONALAUTOCOV" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 21)
  , Input "PC_PERSONALAUTOCOV" (Column "FROZENSETID" "NUMBER" (Just 22) Nothing Nothing Nullable 22)
  , Input "PC_PERSONALAUTOCOV" (Column "EXPIRATIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 23)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM1" "CHAR" (Just 4) Nothing Nothing Nullable 24)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM2" "CHAR" (Just 4) Nothing Nothing Nullable 25)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM3" "CHAR" (Just 4) Nothing Nothing Nullable 26)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM4" "CHAR" (Just 4) Nothing Nothing Nullable 27)
  , Input "PC_PERSONALAUTOCOV" (Column "CREATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 28)
  , Input "PC_PERSONALAUTOCOV" (Column "PALINE" "NUMBER" (Just 22) Nothing Nothing NotNullable 29)
  , Input "PC_PERSONALAUTOCOV" (Column "ARCHIVEPARTITION" "NUMBER" (Just 22) Nothing Nothing Nullable 30)
  , Input "PC_PERSONALAUTOCOV" (Column "BEANVERSION" "NUMBER" (Just 22) Nothing Nothing Nullable 31)
  , Input "PC_PERSONALAUTOCOV" (Column "CHANGETYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 32)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM8AVL" "CHAR" (Just 4) Nothing Nothing Nullable 33)
  , Input "PC_PERSONALAUTOCOV" (Column "BASEDONID" "NUMBER" (Just 22) Nothing Nothing Nullable 34)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM6AVL" "CHAR" (Just 4) Nothing Nothing Nullable 35)
  , Input "PC_PERSONALAUTOCOV" (Column "UPDATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 36)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM4AVL" "CHAR" (Just 4) Nothing Nothing Nullable 37)
  , Input "PC_PERSONALAUTOCOV" (Column "CHOICETERM2AVL" "CHAR" (Just 4) Nothing Nothing Nullable 38)
  , Input "PC_PERSONALAUTOCOV" (Column "REFERENCEDATEINTERNAL" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 39)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM4AVL" "CHAR" (Just 4) Nothing Nothing Nullable 40)
  , Input "PC_PERSONALAUTOCOV" (Column "BOOLEANTERM2AVL" "CHAR" (Just 4) Nothing Nothing Nullable 41)
  , Input "PC_PERSONALAUTOCOV" (Column "SUBTYPE" "NUMBER" (Just 22) Nothing Nothing NotNullable 42)
  , Input "PC_PERSONALAUTOCOV" (Column "PATTERNCODE" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 43)
  , Input "PC_PERSONALAUTOCOV" (Column "BRANCHID" "NUMBER" (Just 22) Nothing Nothing NotNullable 44)
  , Input "PC_POLICY" (Column "PUBLICID" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 1)
  , Input "PC_POLICY" (Column "PRIORPREMIUMS" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 2)
  , Input "PC_POLICY" (Column "ISSUEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 3)
  , Input "PC_POLICY" (Column "PRIORPREMIUMS_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 4)
  , Input "PC_POLICY" (Column "MOVEDPOLICYSOURCEACCOUNTID" "NUMBER" (Just 22) Nothing Nothing Nullable 5)
  , Input "PC_POLICY" (Column "ACCOUNTID" "NUMBER" (Just 22) Nothing Nothing NotNullable 6)
  , Input "PC_POLICY" (Column "CREATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 7)
  , Input "PC_POLICY" (Column "LOSSHISTORYTYPE" "NUMBER" (Just 22) Nothing Nothing NotNullable 8)
  , Input "PC_POLICY" (Column "EXCLUDEDFROMARCHIVE" "CHAR" (Just 4) Nothing Nothing Nullable 9)
  , Input "PC_POLICY" (Column "ARCHIVESTATE" "NUMBER" (Just 22) Nothing Nothing Nullable 10)
  , Input "PC_POLICY" (Column "ARCHIVESCHEMAINFO" "NUMBER" (Just 22) Nothing Nothing Nullable 11)
  , Input "PC_POLICY" (Column "DONOTDESTROY" "CHAR" (Just 4) Nothing Nothing NotNullable 12)
  , Input "PC_POLICY" (Column "ARCHIVEFAILUREDETAILSID" "NUMBER" (Just 22) Nothing Nothing Nullable 13)
  , Input "PC_POLICY" (Column "PACKAGERISK" "NUMBER" (Just 22) Nothing Nothing Nullable 14)
  , Input "PC_POLICY" (Column "NUMPRIORLOSSES" "NUMBER" (Just 22) Nothing Nothing Nullable 15)
  , Input "PC_POLICY" (Column "UPDATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 16)
  , Input "PC_POLICY" (Column "PRIMARYLANGUAGE" "NUMBER" (Just 22) Nothing Nothing Nullable 17)
  , Input "PC_POLICY" (Column "DONOTARCHIVE" "CHAR" (Just 4) Nothing Nothing NotNullable 18)
  , Input "PC_POLICY" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 19)
  , Input "PC_POLICY" (Column "PRIMARYLOCALE" "NUMBER" (Just 22) Nothing Nothing Nullable 20)
  , Input "PC_POLICY" (Column "PRODUCTCODE" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 21)
  , Input "PC_POLICY" (Column "FROZENSETID" "NUMBER" (Just 22) Nothing Nothing Nullable 22)
  , Input "PC_POLICY" (Column "EXCLUDEREASON" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 23)
  , Input "PC_POLICY" (Column "CREATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 24)
  , Input "PC_POLICY" (Column "ARCHIVEFAILUREID" "NUMBER" (Just 22) Nothing Nothing Nullable 25)
  , Input "PC_POLICY" (Column "ORIGINALEFFECTIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 26)
  , Input "PC_POLICY" (Column "BEANVERSION" "NUMBER" (Just 22) Nothing Nothing Nullable 27)
  , Input "PC_POLICY" (Column "ARCHIVEPARTITION" "NUMBER" (Just 22) Nothing Nothing Nullable 28)
  , Input "PC_POLICY" (Column "RETIRED" "NUMBER" (Just 22) Nothing Nothing NotNullable 29)
  , Input "PC_POLICY" (Column "UPDATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 30)
  , Input "PC_POLICY" (Column "PRIORTOTALINCURRED" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 31)
  , Input "PC_POLICY" (Column "ARCHIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 32)
  , Input "PC_POLICY" (Column "PRIORTOTALINCURRED_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 33)
  , Input "PC_POLICY" (Column "PRODUCERCODEOFSERVICEID" "NUMBER" (Just 22) Nothing Nothing NotNullable 34)
  , Input "PC_POLICY" (Column "MOVEDPOLSRCACCTPUBID" "VARCHAR2" (Just 256) Nothing Nothing Nullable 35)
  , Input "PC_POLICYLINE" (Column "SMALLBUSINESSTYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 1)
  , Input "PC_POLICYLINE" (Column "INITIALCOVERAGESCREATED" "CHAR" (Just 4) Nothing Nothing Nullable 2)
  , Input "PC_POLICYLINE" (Column "PUBLICID" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 3)
  , Input "PC_POLICYLINE" (Column "CUSTOMAUTOSYMBOLDESC" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 4)
  , Input "PC_POLICYLINE" (Column "CREATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 5)
  , Input "PC_POLICYLINE" (Column "AUTOSYMBOLSMANUALEDITDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 6)
  , Input "PC_POLICYLINE" (Column "LOCATIONLIMITS" "CHAR" (Just 4) Nothing Nothing Nullable 7)
  , Input "PC_POLICYLINE" (Column "FLEET" "NUMBER" (Just 22) Nothing Nothing Nullable 8)
  , Input "PC_POLICYLINE" (Column "VIEWBUNDLEDCOVERAGES" "CHAR" (Just 4) Nothing Nothing Nullable 9)
  , Input "PC_POLICYLINE" (Column "FIXEDID" "NUMBER" (Just 22) Nothing Nothing NotNullable 10)
  , Input "PC_POLICYLINE" (Column "GLCOVERAGEFORM" "NUMBER" (Just 22) Nothing Nothing Nullable 11)
  , Input "PC_POLICYLINE" (Column "RETROACTIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 12)
  , Input "PC_POLICYLINE" (Column "EFFECTIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 13)
  , Input "PC_POLICYLINE" (Column "UPDATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 14)
  , Input "PC_POLICYLINE" (Column "EQUIPMENTAUTONUMBERSEQ" "NUMBER" (Just 22) Nothing Nothing Nullable 15)
  , Input "PC_POLICYLINE" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 16)
  , Input "PC_POLICYLINE" (Column "EXPIRATIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 17)
  , Input "PC_POLICYLINE" (Column "FROZENSETID" "NUMBER" (Just 22) Nothing Nothing Nullable 18)
  , Input "PC_POLICYLINE" (Column "INITIALEXCLUSIONSCREATED" "CHAR" (Just 4) Nothing Nothing Nullable 19)
  , Input "PC_POLICYLINE" (Column "CREATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 20)
  , Input "PC_POLICYLINE" (Column "POLLUTIONCLEANUPEXP" "CHAR" (Just 4) Nothing Nothing Nullable 21)
  , Input "PC_POLICYLINE" (Column "CLAIMSMADEORIGEFFDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 22)
  , Input "PC_POLICYLINE" (Column "BEANVERSION" "NUMBER" (Just 22) Nothing Nothing Nullable 23)
  , Input "PC_POLICYLINE" (Column "ARCHIVEPARTITION" "NUMBER" (Just 22) Nothing Nothing Nullable 24)
  , Input "PC_POLICYLINE" (Column "GOVERNINGCLASS" "NUMBER" (Just 22) Nothing Nothing Nullable 25)
  , Input "PC_POLICYLINE" (Column "CHANGETYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 26)
  , Input "PC_POLICYLINE" (Column "MANUSCRIPTOPTIONDESC" "CLOB" (Just 4000) Nothing Nothing Nullable 27)
  , Input "PC_POLICYLINE" (Column "POLICYTYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 28)
  , Input "PC_POLICYLINE" (Column "INITIALCONDITIONSCREATED" "CHAR" (Just 4) Nothing Nothing Nullable 29)
  , Input "PC_POLICYLINE" (Column "BASEDONID" "NUMBER" (Just 22) Nothing Nothing Nullable 30)
  , Input "PC_POLICYLINE" (Column "NUMADDINSURED" "NUMBER" (Just 22) Nothing Nothing Nullable 31)
  , Input "PC_POLICYLINE" (Column "UPDATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 32)
  , Input "PC_POLICYLINE" (Column "CPBLANKETAUTONUMBERSEQ" "NUMBER" (Just 22) Nothing Nothing Nullable 33)
  , Input "PC_POLICYLINE" (Column "MANUSCRIPTPREMIUM" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 34)
  , Input "PC_POLICYLINE" (Column "MANUSCRIPTPREMIUM_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 35)
  , Input "PC_POLICYLINE" (Column "SPLITLIMITS" "CHAR" (Just 4) Nothing Nothing Nullable 36)
  , Input "PC_POLICYLINE" (Column "REFERENCEDATEINTERNAL" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 37)
  , Input "PC_POLICYLINE" (Column "PREFERREDCOVERAGECURRENCY" "NUMBER" (Just 22) Nothing Nothing Nullable 38)
  , Input "PC_POLICYLINE" (Column "SUBTYPE" "NUMBER" (Just 22) Nothing Nothing NotNullable 39)
  , Input "PC_POLICYLINE" (Column "BUSINESSVEHICLEAUTONUMBERSEQ" "NUMBER" (Just 22) Nothing Nothing Nullable 40)
  , Input "PC_POLICYLINE" (Column "PERSONALVEHICLEAUTONUMBERSEQ" "NUMBER" (Just 22) Nothing Nothing Nullable 41)
  , Input "PC_POLICYLINE" (Column "BLANKETTYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 42)
  , Input "PC_POLICYLINE" (Column "BRANCHID" "NUMBER" (Just 22) Nothing Nothing NotNullable 43)
  , Input "PC_POLICYLINE" (Column "PATTERNCODE" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 44)
  , Input "PC_POLICYPERIOD" (Column "LOCKED" "CHAR" (Just 4) Nothing Nothing NotNullable 1)
  , Input "PC_POLICYPERIOD" (Column "INVOICINGMETHOD" "NUMBER" (Just 22) Nothing Nothing NotNullable 2)
  , Input "PC_POLICYPERIOD" (Column "EDITEFFECTIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 3)
  , Input "PC_POLICYPERIOD" (Column "ARCHIVESTATE" "NUMBER" (Just 22) Nothing Nothing Nullable 4)
  , Input "PC_POLICYPERIOD" (Column "ARCHIVESCHEMAINFO" "NUMBER" (Just 22) Nothing Nothing Nullable 5)
  , Input "PC_POLICYPERIOD" (Column "LOCATIONAUTONUMBERSEQ" "NUMBER" (Just 22) Nothing Nothing Nullable 6)
  , Input "PC_POLICYPERIOD" (Column "UPDATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 7)
  , Input "PC_POLICYPERIOD" (Column "SINGLECHECKINGPATTERNCODE" "VARCHAR2" (Just 256) Nothing Nothing Nullable 8)
  , Input "PC_POLICYPERIOD" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 9)
  , Input "PC_POLICYPERIOD" (Column "BILLINGMETHOD" "NUMBER" (Just 22) Nothing Nothing Nullable 10)
  , Input "PC_POLICYPERIOD" (Column "CREATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 11)
  , Input "PC_POLICYPERIOD" (Column "QUOTEIDENTIFIER" "VARCHAR2" (Just 240) Nothing Nothing Nullable 12)
  , Input "PC_POLICYPERIOD" (Column "ALLOWGAPSBEFORE" "CHAR" (Just 4) Nothing Nothing NotNullable 13)
  , Input "PC_POLICYPERIOD" (Column "QUOTEHIDDEN" "CHAR" (Just 4) Nothing Nothing NotNullable 14)
  , Input "PC_POLICYPERIOD" (Column "BEANVERSION" "NUMBER" (Just 22) Nothing Nothing Nullable 15)
  , Input "PC_POLICYPERIOD" (Column "BRANCHNAME" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 16)
  , Input "PC_POLICYPERIOD" (Column "UPDATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 17)
  , Input "PC_POLICYPERIOD" (Column "CANCELLATIONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 18)
  , Input "PC_POLICYPERIOD" (Column "SEGMENT" "NUMBER" (Just 22) Nothing Nothing Nullable 19)
  , Input "PC_POLICYPERIOD" (Column "PRIMARYINSUREDNAME" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 20)
  , Input "PC_POLICYPERIOD" (Column "TEMPORARYBRANCH" "CHAR" (Just 4) Nothing Nothing NotNullable 21)
  , Input "PC_POLICYPERIOD" (Column "DEPOSITOVERRIDEPCT" "NUMBER" (Just 22) (Just 12) (Just 3) Nullable 22)
  , Input "PC_POLICYPERIOD" (Column "POLICYTERMID" "NUMBER" (Just 22) Nothing Nothing NotNullable 23)
  , Input "PC_POLICYPERIOD" (Column "SELECTEDTERMTYPE" "NUMBER" (Just 22) Nothing Nothing Nullable 24)
  , Input "PC_POLICYPERIOD" (Column "PERIODSTART" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 25)
  , Input "PC_POLICYPERIOD" (Column "ALTBILLINGACCOUNTNUMBER" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 26)
  , Input "PC_POLICYPERIOD" (Column "PUBLICID" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 27)
  , Input "PC_POLICYPERIOD" (Column "WRITTENDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 28)
  , Input "PC_POLICYPERIOD" (Column "TOTALCOSTRPT" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 29)
  , Input "PC_POLICYPERIOD" (Column "TOTALCOSTRPT_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 30)
  , Input "PC_POLICYPERIOD" (Column "MOSTRECENTMODEL" "CHAR" (Just 4) Nothing Nothing NotNullable 31)
  , Input "PC_POLICYPERIOD" (Column "ALLOCATIONOFREMAINDER" "NUMBER" (Just 22) Nothing Nothing Nullable 32)
  , Input "PC_POLICYPERIOD" (Column "OVERRIDEBILLINGALLOCATION" "CHAR" (Just 4) Nothing Nothing Nullable 33)
  , Input "PC_POLICYPERIOD" (Column "ARCHIVEFAILUREDETAILSID" "NUMBER" (Just 22) Nothing Nothing Nullable 34)
  , Input "PC_POLICYPERIOD" (Column "MODELDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 35)
  , Input "PC_POLICYPERIOD" (Column "INVOICESTREAMCODE" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 36)
  , Input "PC_POLICYPERIOD" (Column "TAXSURCHARGESRPT_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 37)
  , Input "PC_POLICYPERIOD" (Column "FROZENSETID" "NUMBER" (Just 22) Nothing Nothing Nullable 38)
  , Input "PC_POLICYPERIOD" (Column "BASESTATE" "NUMBER" (Just 22) Nothing Nothing Nullable 39)
  , Input "PC_POLICYPERIOD" (Column "MODELNUMBERINDEX" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 40)
  , Input "PC_POLICYPERIOD" (Column "MOSTRECENTMODELINDEX" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 41)
  , Input "PC_POLICYPERIOD" (Column "ARCHIVEPARTITION" "NUMBER" (Just 22) Nothing Nothing Nullable 42)
  , Input "PC_POLICYPERIOD" (Column "FAILEDOOSEEVALUATION" "CHAR" (Just 4) Nothing Nothing Nullable 43)
  , Input "PC_POLICYPERIOD" (Column "BRANCHNUMBER" "NUMBER" (Just 22) Nothing Nothing Nullable 44)
  , Input "PC_POLICYPERIOD" (Column "TRANSACTIONCOSTRPT" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 45)
  , Input "PC_POLICYPERIOD" (Column "DEPOSITCOLLECTED" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 46)
  , Input "PC_POLICYPERIOD" (Column "TRANSACTIONCOSTRPT_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 47)
  , Input "PC_POLICYPERIOD" (Column "DEPOSITCOLLECTED_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 48)
  , Input "PC_POLICYPERIOD" (Column "BASEDONID" "NUMBER" (Just 22) Nothing Nothing Nullable 49)
  , Input "PC_POLICYPERIOD" (Column "ARCHIVEDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 50)
  , Input "PC_POLICYPERIOD" (Column "BILLIMMEDIATELYPERCENTAGE" "NUMBER" (Just 22) (Just 4) (Just 1) Nullable 51)
  , Input "PC_POLICYPERIOD" (Column "QUOTECLONEORIGINALPERIOD" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 52)
  , Input "PC_POLICYPERIOD" (Column "DEPOSITAMOUNT" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 53)
  , Input "PC_POLICYPERIOD" (Column "PREFERREDCOVERAGECURRENCY" "NUMBER" (Just 22) Nothing Nothing NotNullable 54)
  , Input "PC_POLICYPERIOD" (Column "PERIODEND" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 55)
  , Input "PC_POLICYPERIOD" (Column "PREFERREDSETTLEMENTCURRENCY" "NUMBER" (Just 22) Nothing Nothing NotNullable 56)
  , Input "PC_POLICYPERIOD" (Column "BASEDONDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 57)
  , Input "PC_POLICYPERIOD" (Column "TOTALPREMIUMRPT" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 58)
  , Input "PC_POLICYPERIOD" (Column "TOTALPREMIUMRPT_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 59)
  , Input "PC_POLICYPERIOD" (Column "VALIDREINSURANCE" "CHAR" (Just 4) Nothing Nothing Nullable 60)
  , Input "PC_POLICYPERIOD" (Column "SERIESCHECKINGPATTERNCODE" "VARCHAR2" (Just 256) Nothing Nothing Nullable 61)
  , Input "PC_POLICYPERIOD" (Column "PNICONTACTDENORM" "NUMBER" (Just 22) Nothing Nothing Nullable 62)
  , Input "PC_POLICYPERIOD" (Column "DONOTDESTROY" "CHAR" (Just 4) Nothing Nothing NotNullable 63)
  , Input "PC_POLICYPERIOD" (Column "EDITLOCKED" "CHAR" (Just 4) Nothing Nothing NotNullable 64)
  , Input "PC_POLICYPERIOD" (Column "QUOTEMATURITYLEVEL" "NUMBER" (Just 22) Nothing Nothing NotNullable 65)
  , Input "PC_POLICYPERIOD" (Column "RATEASOFDATE" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) Nullable 66)
  , Input "PC_POLICYPERIOD" (Column "JOBID" "NUMBER" (Just 22) Nothing Nothing Nullable 67)
  , Input "PC_POLICYPERIOD" (Column "UWCOMPANY" "NUMBER" (Just 22) Nothing Nothing Nullable 68)
  , Input "PC_POLICYPERIOD" (Column "ESTIMATEDPREMIUM" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 69)
  , Input "PC_POLICYPERIOD" (Column "PERIODID" "NUMBER" (Just 22) Nothing Nothing NotNullable 70)
  , Input "PC_POLICYPERIOD" (Column "ESTIMATEDPREMIUM_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 71)
  , Input "PC_POLICYPERIOD" (Column "ASSIGNEDRISK" "CHAR" (Just 4) Nothing Nothing Nullable 72)
  , Input "PC_POLICYPERIOD" (Column "TRANSACTIONPREMIUMRPT" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 73)
  , Input "PC_POLICYPERIOD" (Column "SPECIALHANDLING" "NUMBER" (Just 22) Nothing Nothing Nullable 74)
  , Input "PC_POLICYPERIOD" (Column "TEMPORARYCLONESTATUS" "NUMBER" (Just 22) Nothing Nothing Nullable 75)
  , Input "PC_POLICYPERIOD" (Column "TRANSACTIONPREMIUMRPT_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 76)
  , Input "PC_POLICYPERIOD" (Column "EXCLUDEREASON" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 77)
  , Input "PC_POLICYPERIOD" (Column "ARCHIVEFAILUREID" "NUMBER" (Just 22) Nothing Nothing Nullable 78)
  , Input "PC_POLICYPERIOD" (Column "FAILEDOOSEVALIDATION" "CHAR" (Just 4) Nothing Nothing Nullable 79)
  , Input "PC_POLICYPERIOD" (Column "RETIRED" "NUMBER" (Just 22) Nothing Nothing NotNullable 80)
  , Input "PC_POLICYPERIOD" (Column "PREEMPTED" "CHAR" (Just 4) Nothing Nothing NotNullable 81)
  , Input "PC_POLICYPERIOD" (Column "FUTUREPERIODS" "CHAR" (Just 4) Nothing Nothing NotNullable 82)
  , Input "PC_POLICYPERIOD" (Column "PRIMARYINSUREDNAMEDENORM" "VARCHAR2" (Just 1020) Nothing Nothing Nullable 83)
  , Input "PC_POLICYPERIOD" (Column "MODELNUMBER" "NUMBER" (Just 22) Nothing Nothing Nullable 84)
  , Input "PC_POLICYPERIOD" (Column "TERMNUMBER" "NUMBER" (Just 22) Nothing Nothing Nullable 85)
  , Input "PC_POLICYPERIOD" (Column "WAIVEDEPOSITCHANGE" "CHAR" (Just 4) Nothing Nothing Nullable 86)
  , Input "PC_POLICYPERIOD" (Column "PRODUCERCODEOFRECORDID" "NUMBER" (Just 22) Nothing Nothing NotNullable 87)
  , Input "PC_POLICYPERIOD" (Column "CREATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 88)
  , Input "PC_POLICYPERIOD" (Column "POLICYID" "NUMBER" (Just 22) Nothing Nothing NotNullable 89)
  , Input "PC_POLICYPERIOD" (Column "EXCLUDEDFROMARCHIVE" "CHAR" (Just 4) Nothing Nothing Nullable 90)
  , Input "PC_POLICYPERIOD" (Column "TAXSURCHARGESRPT" "NUMBER" (Just 22) (Just 18) (Just 2) Nullable 91)
  , Input "PC_POLICYPERIOD" (Column "QUOTECLONESEQUENCENUMBER" "NUMBER" (Just 22) Nothing Nothing Nullable 92)
  , Input "PC_POLICYPERIOD" (Column "LOCKINGCOLUMN" "NUMBER" (Just 22) Nothing Nothing Nullable 93)
  , Input "PC_POLICYPERIOD" (Column "REFUNDCALCMETHOD" "NUMBER" (Just 22) Nothing Nothing Nullable 94)
  , Input "PC_POLICYPERIOD" (Column "STATUS" "NUMBER" (Just 22) Nothing Nothing NotNullable 95)
  , Input "PC_POLICYPERIOD" (Column "DEPOSITAMOUNT_CUR" "NUMBER" (Just 22) Nothing Nothing Nullable 96)
  , Input "PC_POLICYPERIOD" (Column "POLICYNUMBER" "VARCHAR2" (Just 160) Nothing Nothing Nullable 97)
  , Input "PC_UWCOMPANY" (Column "CREATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 1)
  , Input "PC_UWCOMPANY" (Column "PUBLICID" "VARCHAR2" (Just 256) Nothing Nothing NotNullable 2)
  , Input "PC_UWCOMPANY" (Column "BEANVERSION" "NUMBER" (Just 22) Nothing Nothing Nullable 3)
  , Input "PC_UWCOMPANY" (Column "RETIRED" "NUMBER" (Just 22) Nothing Nothing NotNullable 4)
  , Input "PC_UWCOMPANY" (Column "CREATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 5)
  , Input "PC_UWCOMPANY" (Column "NAME" "VARCHAR2" (Just 400) Nothing Nothing NotNullable 6)
  , Input "PC_UWCOMPANY" (Column "CODE" "NUMBER" (Just 22) Nothing Nothing NotNullable 7)
  , Input "PC_UWCOMPANY" (Column "UPDATEUSERID" "NUMBER" (Just 22) Nothing Nothing Nullable 8)
  , Input "PC_UWCOMPANY" (Column "STATE" "NUMBER" (Just 22) Nothing Nothing Nullable 9)
  , Input "PC_UWCOMPANY" (Column "STATUS" "NUMBER" (Just 22) Nothing Nothing NotNullable 10)
  , Input "PC_UWCOMPANY" (Column "PARENTNAME" "VARCHAR2" (Just 400) Nothing Nothing NotNullable 11)
  , Input "PC_UWCOMPANY" (Column "UPDATETIME" "TIMESTAMP(6)" (Just 11) Nothing (Just 6) NotNullable 12)
  , Input "PC_UWCOMPANY" (Column "NAICCODE" "VARCHAR2" (Just 40) Nothing Nothing Nullable 13)
  , Input "PC_UWCOMPANY" (Column "ID" "NUMBER" (Just 22) Nothing Nothing NotNullable 14)
  ]
