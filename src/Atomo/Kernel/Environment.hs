{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Environment where

import System.Environment

import Atomo.Environment
import Atomo.Haskell


load :: VM ()
load = do
    ([$p|Environment|] =::) =<< eval [$e|Object clone|]

    [$p|Environment arguments|] =:
        liftIO getArgs >>= list . map string

    [$p|Environment program-name|] =:
        liftM string $ liftIO getProgName

    [$p|Environment get: (name: String)|] =:
        getString [$e|name|]
            >>= liftM string . (liftIO . getEnv)

    [$p|Environment all|] =: do
        env <- liftIO getEnvironment

        assocs <- forM env $ \(k, v) ->
            dispatch (keyword ["->"] [string k, string v])

        list assocs
