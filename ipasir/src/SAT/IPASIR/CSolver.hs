module SAT.IPASIR.CSolver where

import Control.Monad

import SAT.IPASIR.Literals

class CSolver a where
    ipasirSignature :: a -> IO String
    ipasirInit   :: IO a
    ipasirAdd    :: Maybe (Lit Word) -> a -> IO ()
    ipasirAssume :: Lit Word -> a -> IO ()
    ipasirSolve  :: a -> IO (Maybe Bool)
    ipasirVal    :: Word -> a -> IO (Maybe (Lit Word))
    ipasirFailed :: Word -> a -> IO Bool
    
    ipasirAddClause :: [Lit Word] -> a -> IO ()
    ipasirAddClause [] s = ipasirAdd Nothing s
    ipasirAddClause (l:ls) s = do
        ipasirAdd (Just l) s
        ipasirAddClause ls s
    
    ipasirAddClauses :: [[Lit Word]] -> a -> IO ()
    ipasirAddClauses [] s = return ()
    ipasirAddClauses (l:ls) s = do
        ipasirAddClause  l s
        ipasirAddClauses ls s

    --ipasir_set_terminate :: a ->  (void * solver, void * state, int (*terminate)(void * state));
