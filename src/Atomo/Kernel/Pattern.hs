{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Pattern (load) where

import Atomo
import Atomo.Pattern (match)


load :: VM ()
load = do
    ([$p|Pattern Role|] =::) =<< eval [$e|Pattern clone|]
    ([$p|Pattern Define|] =::) =<< eval [$e|Pattern clone|]

    [$p|(e: Expression) as: Pattern|] =: do
        (e :: Expr) <- here "e" >>= getV
        p <- toPattern' e
        return (Pattern p)

    [$p|(e: Expression) as: Pattern Role|] =: do
        (e :: Expr) <- here "e" >>= getV
        p <- toRolePattern' e
        return (Pattern p)

    [$p|(e: Expression) as: Pattern Define|] =: do
        (e :: Expr) <- here "e" >>= getV
        p <- toDefinePattern' e
        return (Pattern (PMessage p))

    [$p|(p: Pattern) name|] =: do
        (p :: Pattern) <- here "p" >>= getV

        case p of
            PNamed n _ -> return (string n)
            PMessage (Single { mName = n }) -> return (string n)
            _ -> raise ["no-name-for"] [Pattern p]

    [$p|(p: Pattern) names|] =: do
        (p :: Pattern) <- here "p" >>= getV

        case p of
            PMessage (Keyword { mNames = ns }) -> return $ list (map string ns)
            _ -> raise ["no-names-for"] [Pattern p]

    [$p|(p: Pattern) target|] =: do
        (p :: Pattern) <- here "p" >>= getV

        case p of
            PMessage (Single { mTarget = t }) -> return (Pattern t)
            _ -> raise ["no-target-for"] [Pattern p]

    [$p|(p: Pattern) targets|] =: do
        (p :: Pattern) <- here "p" >>= getV

        case p of
            PMessage (Keyword { mTargets = ts }) -> return $ list (map Pattern ts)
            _ -> raise ["no-targets-for"] [Pattern p]

    [$p|(p: Pattern) matches?: v|] =: do
        (p :: Pattern) <- here "p" >>= getV
        v <- here "v"
        ids <- gets primitives

        if match ids Nothing p v
            then do
                bs <- eval [$e|Object clone|]
                withTop bs (set p v)
                return (keyParticle ["yes"] [Nothing, Just bs])
            else return (particle "no")

    [$p|top match: (p: Pattern) on: v|] =: do
        p <- here "p" >>= getV >>= matchable' . fromPattern
        v <- here "v"
        t <- here "top"

        case p of
            PMessage m -> define m (Primitive Nothing v) >> return v
            _          -> withTop t (set p v)
