{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Particle (load) where

import Atomo


load :: VM ()
load = do
    [$p|(p: Particle) call: (targets: List)|] =:::
        [$e|(p complete: targets) dispatch|]

    [$p|(p: Particle) name|] =: do
        ((PMSingle n :: Particle Value)) <- here "p" >>= getV
        return (string n)

    [$p|(p: Particle) names|] =: do
        ((PMKeyword ns _ :: Particle Value)) <- here "p" >>= getV
        return $ list (map string ns)

    [$p|(p: Particle) values|] =: do
        ((PMKeyword _ mvs) :: Particle Value) <- here "p" >>= getV
        return . list $
            map
                (maybe (particle "none") (keyParticleN ["ok"] . (:[])))
                mvs

    [$p|(p: Particle) type|] =: do
        (p :: Particle Value) <- here "p" >>= getV
        case p of
            PMKeyword {} -> return (particle "keyword")
            PMSingle {} -> return (particle "single")

    [$p|(p: Particle) complete: (targets: List)|] =: do
        (p :: Particle Value) <- here "p" >>= getV
        vs <- getList [$e|targets|]

        case p of
            PMKeyword ns mvs ->
                let blanks = length (filter (== Nothing) mvs)
                in
                    if blanks > length vs
                        then throwError (ParticleArity blanks (length vs))
                        else return . Message . keyword ns $ completeKP mvs vs
            PMSingle n ->
                if null vs
                    then throwError (ParticleArity 1 0)
                    else return . Message . single n $ head vs

    [$p|c define: (p: Particle) on: v with: (targets: List) as: e|] =: do
        (p :: Particle Value) <- here "p" >>= getV
        v <- here "v"
        ts <- getList [$e|targets|]
        e <- here "e"
        c <- here "c"

        let toPattern (Pattern p) = p
            toPattern v = PMatch v
            
            others = map toPattern ts
            
            main = toPattern v

        ids <- gets primitives
        obj <- targets' ids main

        pat <-
            matchable $
                case p of
                    PMKeyword ns _ ->
                        keyword ns (main:others)
                    PMSingle n ->
                        single n main

        let m =
                case e of
                    Expression e' -> Responder pat c e'
                    _ -> Slot pat v

        forM_ obj $ \o ->
            defineOn (Reference o) m
        
        return (particle "ok")

    [$p|c define: (p: Particle) on: (targets: List) as: v|] =: do
        (p :: Particle Value) <- here "p" >>= getV
        vs <- getList [$e|targets|]
        v <- here "v"
        c <- here "c"

        let targets =
                map (\v ->
                    case v of
                        Pattern p -> p
                        _ -> PMatch v) vs
            expr =
                case v of
                    Expression e -> e
                    _ -> Primitive Nothing v

        withTop c $ do
            case p of
                PMKeyword ns _ ->
                    define (keyword ns targets) expr

                PMSingle n ->
                    define (single n (head targets)) expr

            return (particle "ok")
