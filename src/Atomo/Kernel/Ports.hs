{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Ports (load) where

import Data.Dynamic
import System.IO
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Haskell
import Atomo.Pretty


load :: VM ()
load = do
    port <- eval [$e|Object clone|]
    [$p|Port|] =:: port

    sinp <- portObj stdin
    soutp <- portObj stdout
    serrp <- portObj stderr
    [$p|Port standard-input|] =:: sinp
    [$p|Port standard-output|] =:: soutp
    [$p|Port standard-error|] =:: serrp

    [$p|current-output-port|] =:: soutp
    [$p|current-input-port|] =:: sinp

    [$p|Port new: (fn: String)|] =::: [$e|Port new: fn mode: @read-write|]
    [$p|Port new: (fn: String) mode: (m: Particle)|] =: do
        fn <- fmap (map (\(Char c) -> c) . V.toList) (getList [$e|fn|])
        Particle m <- here "m" >>= findValue isParticle

        hdl <- case m of
            PMSingle "read" ->
                liftIO (openFile fn ReadMode)
            PMSingle "write" ->
                liftIO (openFile fn WriteMode)
            PMSingle "append" ->
                liftIO (openFile fn AppendMode)
            PMSingle "read-write" ->
                liftIO (openFile fn ReadWriteMode)
            _ ->
                error $ "unknown port mode: " ++ show (pretty m) ++ ", must be one of: @read, @write, @append, @read-write"

        portObj hdl

    [$p|(p: Port) flush|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hFlush (fromDyn hdl (error "port handle invalid!"))) -- TODO
        return (particle "ok")

    [$p|(p: Port) close|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hClose (fromDyn hdl (error "port handle invalid!"))) -- TODO
        return (particle "ok")

    [$p|(p: Port) open?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsOpen (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) readable?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsReadable (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) writable?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsWritable (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) seekable?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsSeekable (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) closed?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsClosed (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) ready?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hReady (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(p: Port) eof?|] =: do
        Haskell hdl <- eval [$e|p handle|]
        liftIO (hIsEOF (fromDyn hdl (error "port handle invalid!"))) -- TODO
            >>= bool

    [$p|(x: Object) print|] =: do
        x <- here "x"
        Haskell h <- eval [$e|dispatch sender current-output-port handle|]

        cs <- fmap V.toList $ getList [$e|x as: String|]

        let hdl = fromDyn h (error "current-output-port handle invalid!")

        if all isChar cs
            then do
                liftIO (hPutStrLn hdl (map (\(Char c) -> c) cs))
                liftIO (hFlush hdl)
                return x
            else throwError $ ErrorMsg "@as:String returned non-String"

    [$p|read-line|] =: do
        Haskell inh <- eval [$e|dispatch sender current-input-port handle|]
        line <- liftIO (hGetLine (fromDyn inh (error "current-input-port handle invalid!"))) -- TODO
        string line
  where
    portObj hdl = newScope $ do
        port <- eval [$e|Port clone|]
        [$p|p|] =:: port
        [$p|p handle|] =:: Haskell (toDyn hdl)
        here "p"
