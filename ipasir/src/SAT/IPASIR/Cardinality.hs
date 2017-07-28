module SAT.IPASIR.Cardinality where

data AMOConfig v = AtMostOneConfig     [Lit v]
data AMKConfig v = AtMostKConfig       [Lit v] Int
data PBConfig  v = PseudoBooleanConfig [(Int,Lit v)] (Maybe Int) (Maybe Int)

class AtMostOne card v where
    getLiterals          :: card v -> [Lit v]
    atMostOneCardinality :: [Lit v] -> card v

class AtMostOne card v => AtMostK card v where
    getK :: AtMostKConfig v => Int
    atMostKCardinality :: [Lit v] -> Int -> card v

class AtMostK card v => PseudoBoolen card v where
    getWeightedLits :: card v -> [(Lit v,Int)]
    pseudoBooleanCardinality :: [(Lit v,Int)] -> Int -> card v


