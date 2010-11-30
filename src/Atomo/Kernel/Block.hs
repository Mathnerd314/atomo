{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Kernel.Block (load) where

import Atomo


load :: VM ()
load = do
    [$p|Block new: (es: List) in: t|] =:::
        [$e|Block new: es arguments: [] in: t|]

    [$p|Block new: (es: List) arguments: (as: List) in: t|] =: do
        t <- here "t"
        es <- getList [$e|es|]
        as <- getList [$e|as|]

        return (Block (t, map fromPattern as, map fromExpression es))

    [$p|(b: Block) call|] =: do
        b <- here "b" >>= getV
        callBlock b []

    [$p|(b: Block) repeat|] =: do
        b@(c, _, _) <- here "b" >>= getV
        withTop c (forever (callBlock b []))

    [$p|(b: Block) call: (l: List)|] =: do
        b <- here "b" >>= getV
        vs <- getList [$e|l|]
        callBlock b vs

    [$p|(b: Block) call-in: c|] =: do
        ((_, _, es) :: Block) <- here "b" >>= getV
        c <- here "c"
        withTop c (evalAll es)

    [$p|(b: Block) context|] =: do
        ((s, _, _) :: Block) <- here "b" >>= getV
        return s

    [$p|(b: Block) arguments|] =: do
        ((_, as, _) :: Block) <- here "b" >>= getV
        return $ list (map Pattern as)

    [$p|(b: Block) contents|] =: do
        ((_, _, es) :: Block) <- here "b" >>= getV
        return $ list (map Expression es)
