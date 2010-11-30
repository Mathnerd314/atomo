{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Atomo.Kernel.Method where

import Atomo


load :: VM ()
load = do
    [$p|(m: Method) value|] =: do
        (m :: Method) <- here "m" >>= getV
        return (mValue m)

    [$p|(m: Method) pattern|] =: do
        (m :: Method) <- here "m" >>= getV
        return (Pattern (PMessage (mPattern m)))

    [$p|(m: Method) expression|] =: do
        (m :: Method) <- here "m" >>= getV
        return (Expression (mExpr m))

    [$p|(m: Method) context|] =: do
        (m :: Method) <- here "m" >>= getV
        return (mContext m)
