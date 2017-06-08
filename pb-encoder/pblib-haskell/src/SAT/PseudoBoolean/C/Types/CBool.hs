module SAT.PseudoBoolean.C.Types.CBool where

import Foreign.C.Types

type CBool = CInt
cBool :: Bool -> CInt
cBool True = true
cBool False = false
true :: Num a => a
true = 1
false :: Num a => a
false = 0
