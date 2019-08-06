module Ddl where

data Nullability
  = Nullable
  | NotNullable
  deriving (Show, Eq)

data Column =
  Column
    { cName :: String
    , cType :: String
    , cLength :: Maybe Integer
    , cPrecision :: Maybe Integer
    , cScale :: Maybe Integer
    , cNullability :: Nullability
    , cId :: Integer
    }
  deriving (Show, Eq)

data Input =
  Input
    { riName :: String
    , riColumn :: Column
    }
  deriving (Show, Eq)

data Relation =
  Relation
    { rName :: String
    , rColumns :: [Column]
    , rPk :: Maybe PkConstraint
    }
  deriving (Show, Eq)

{-
data FromTo =
  FromTo
    { ftFrom :: Relation
    , ftTo :: Relation
    }
    deriving (Eq,
, Sh
data Topicity
  = From Relation
  | Join Relation Relation
  | LeftJoin Relation Relation
-}
data PkConstraint =
  PkConstraint
    { pkcTableName :: String
    , pkcColumnName :: String
    , pkcConstraintName :: String
    }
  deriving (Show, Eq)
