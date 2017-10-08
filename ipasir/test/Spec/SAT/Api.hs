import SAT.IPASIR.Api
import SAT.IPASIR.Cryptominisat 
import Debug.Trace

cnf1 = [[-1,2,5],[-5,4],[2,3],[1,2,3,4,5],[4,-2],[-2,1]]

testAllSolutionsIn = do
    solver <- ipasirInit :: IO CryptoMiniSat
    ipasirAddClauses solver cnf1
    (c,s) <- ipasirAllSolutions solver
    print $ s !! 0
    c' <- ipasirConflict solver
    print c'
    --print c
    return ()


myTestSeq :: (Show a, Show b) => a -> b -> b
myTestSeq x y = trace ("seq " ++ show x ++ " (") $ y

