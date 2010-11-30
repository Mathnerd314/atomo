{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Atomo.Kernel.Concurrency (load) where

import Atomo
import Atomo.Method
import Atomo.Spawn


load :: VM ()
load = do
    [$p|self|] =: do
        chan <- gets channel
        tid <- liftIO myThreadId
        return (Process (chan,tid))

    [$p|receive|] =: gets channel >>= liftIO . readChan

    [$p|halt|] =: gets halt >>= liftIO >> return (particle "ok")

    [$p|(p: Process) <- v|] =: do
        ((chan, _) :: Process) <- here "p" >>= getV
        v <- here "v"
        liftIO (writeChan chan v)
        here "p"

    [$p|(b: Block) spawn|] =: do
        ((s, as, bes) :: Block) <- here "b" >>= getV

        if length as > 0
            then throwError (BlockArity (length as) 0)
            else spawn (doBlock emptyMap s bes)

    [$p|(b: Block) spawn: (l: List)|] =: do
        (b@(_, as, _) :: Block) <- here "b" >>= getV
        vs <- getList [$e|l|]

        if length as > length vs
            then throwError (BlockArity (length as) (length vs))
            else spawn (callBlock b vs)

    [$p|(p: Process) stop|] =: do
        ((_, tid) :: Process) <- here "p" >>= getV
        liftIO (killThread tid)
        return (particle "ok")
