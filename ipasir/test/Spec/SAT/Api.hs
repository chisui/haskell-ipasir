import SAT.IPASIR.Api
import SAT.IPASIR.Cryptominisat 

cnf1 = [[-1,2,5],[-5,4],[2,3],[1,2,3,4,5],[4,-2],[-2,1]]

testAllSolutionsIn = do
    solver <- ipasirInit :: IO CryptoMiniSat
    ipasirAddClauses solver cnf1
    solutions <- ipasirAllSolutionsC solver
    --print solutions
    return ()

