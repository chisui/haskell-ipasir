{-# LANGUAGE DeriveGeneric #-}
module SAT.PseudoBoolean.Config where

import GHC.Generics

defaultConfig ::Config c
defaultConfig = Config
    { approximate = Nothing
    , useFormulaCache = False
    , checkForDuplicateLiterals = False
    , config = Nothing
    }

data Config c = Config
    { approximate :: Maybe Word
    , useFormulaCache :: Bool
    , checkForDuplicateLiterals :: Bool
    , config ::Maybe c
    } deriving (Eq, Show, Read, Generic)

data BimanderMIs
    = Half
    | Sqrt
    | Fixed
        deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

data AtMostOne
    = AmoNested
    | AmoBdd
        { amoRecursive :: Bool
        , amoRealdRoBdds :: Bool
        }
    | AmoBimander
        { mIs :: BimanderMIs
        , m :: Word
        }
    | AmoCommander { k :: Word }
    | AmoKProduct
        { splitThreshold :: Word
        , chunkSize :: Word
        }
    | AmoBinary
    | AmoPairwise
        deriving (Eq, Show, Read, Generic)

data AtMostK
    = AmkBdd
        { amkRecursive :: Bool
        , amkRealdRoBdds :: Bool
        }
    | AmkCard
        deriving (Eq, Show, Read, Generic)

data PseudoBoolean
    = PbBdd
    | PbSequentialWeightCounter
    | PbSortingNetworks
    | PbAdder
    | PbBinaryMerge
        { useGac :: Bool
        , noSupportForSingeBits :: Bool
        }
        deriving (Eq, Show, Read, Generic)
