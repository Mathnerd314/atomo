{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Atomo.Kernel.Message (load) where

import Atomo


load :: VM ()
load = do
    [$p|(m: Message) type|] =: do
        (m :: Message Value) <- here "m" >>= getV
        case m of
            Single {} -> return (particle "single")
            Keyword {} -> return (particle "keyword")

    [$p|(m: Message) dispatch|] =:
        here "m" >>= getV >>= dispatch . fromMessage

    [$p|(m: Message) particle|] =: do
        (m :: Message Value) <- here "m" >>= getV
        case m of
            Single { mName = n } -> return (particle n)
            Keyword { mNames = ns } -> return (keyParticle ns (replicate (length ns + 1) Nothing))

    [$p|(m: Message) target|] =: do
        ((Single { mTarget = t }) :: Message Value) <- here "m" >>= getV
        return t

    [$p|(m: Message) targets|] =: do
        ((Keyword { mTargets = ts }) :: Message Value) <- here "m" >>= getV
        return $ list ts
