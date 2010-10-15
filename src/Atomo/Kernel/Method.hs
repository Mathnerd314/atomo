{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Method where

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    [$p|(m: Method) value|] =: do
        Method m <- here "m" >>= findMethod'
        return (mValue m)

    [$p|(m: Method) pattern|] =: do
        Method m <- here "m" >>= findMethod'
        return (Pattern (mPattern m))

    [$p|(m: Method) expression|] =: do
        Method m <- here "m" >>= findMethod'
        return (Expression (mExpr m))

    [$p|(m: Method) context|] =: do
        Method m <- here "m" >>= findMethod'
        return (mContext m)
