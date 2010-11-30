{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Atomo.Kernel.Numeric (load) where

import Data.Ratio

import Atomo


load :: VM ()
load = do
    mapM_ eval
        [ [$e|operator right 8 ^|]
        , [$e|operator 7 % * /|]
        , [$e|operator 6 + -|]
        ]

    eval [$e|Object clone|] >>= ([$p|Number|] =::)

    eval [$e|Integer delegates-to: Number|]
    eval [$e|Double delegates-to: Number|]

    [$p|(a: Integer) sqrt|] =: lift1 (sqrt . fromInteger :: Integer -> Double)

    [$p|(a: Double) sqrt|] =: lift1 (sqrt :: Double -> Double)

    [$p|(a: Double) ceiling|] =: lift1 (ceiling :: Double -> Integer)

    [$p|(a: Double) round|] =: lift1 (round :: Double -> Integer)

    [$p|(a: Double) floor|] =: lift1 (floor :: Double -> Integer)

    [$p|(a: Integer) reciprocal|] =: lift1 (recip . fromInteger :: Integer -> Double)

    [$p|(a: Double) reciprocal|] =: lift1 (recip :: Double -> Double)

    [$p|(a: Rational) reciprocal|] =: lift1 (recip :: Rational -> Rational)

    [$p|(a: Rational) numerator|] =: lift1 (numerator :: Rational -> Integer)

    [$p|(a: Rational) denominator|] =: lift1 (denominator :: Rational -> Integer)

    [$p|(d: Double) as: Integer|] =::: [$e|d floor|]
    [$p|(d: Double) as: Rational|] =::: [$e|d rationalize|]
    [$p|(a: Integer) as: Double|] =: lift1 (fromInteger :: Integer -> Double)
    [$p|(a: Integer) as: Rational|] =: lift1 ((%1) :: Integer -> Rational)
    [$p|(r: Rational) as: Double|] =::: [$e|r approximate|]
    [$p|(r: Rational) as: Integer|] =::: [$e|r approximate floor|]

    [$p|(i: Integer) rationalize|] =::: [$e|(i as: Double) rationalize|]
    [$p|(d: Double) rationalize|] =::: [$e|d rationalize: 0.001|]
    [$p|(a: Double) rationalize: (b: Double)|] =: lift2 (approxRational :: Double -> Double -> Rational)

    [$p|(a: Rational) approximate|] =: lift1 (fromRational :: Rational -> Double)

    [$p|(a: Integer) + (b: Integer)|] =: lift2 ((+) :: Integer -> Integer -> Integer)
    [$p|(a: Rational) + (b: Rational)|] =: lift2 ((+) :: Rational -> Rational -> Rational)
    [$p|(a: Double) + (b: Double)|] =: lift2 ((+) :: Double -> Double -> Double)
    [$p|(a: Integer) + (b: Double)|] =: lift2 ((.fromInteger) (+) :: Integer -> Double -> Double)
    [$p|(a: Integer) + (b: Rational)|] =: lift2 ((.toRational) (+) :: Integer -> Rational -> Rational)
    [$p|(a: Double) + (b: Integer)|] =: lift2 ((\f a b -> f a (fromInteger b)) (+) :: Double -> Integer -> Double)
    [$p|(a: Double) + (b: Rational)|] =: lift2 ((\f a b -> f (toRational a) b) (+) :: Double -> Rational -> Rational)
    [$p|(a: Rational) + (b: Integer)|] =: lift2 ((\f a b -> f a (toRational b)) (+) :: Rational -> Integer -> Rational)
    [$p|(a: Rational) + (b: Double)|] =: lift2 ((\f a b -> f a (toRational b)) (+) :: Rational -> Double -> Rational)

    [$p|(a: Integer) - (b: Integer)|] =: lift2 ((-) :: Integer -> Integer -> Integer)
    [$p|(a: Rational) - (b: Rational)|] =: lift2 ((-) :: Rational -> Rational -> Rational)
    [$p|(a: Double) - (b: Double)|] =: lift2 ((-) :: Double -> Double -> Double)
    [$p|(a: Integer) - (b: Double)|] =: lift2 ((\f a b -> f (fromInteger a) b) (-) :: Integer -> Double -> Double)
    [$p|(a: Integer) - (b: Rational)|] =: lift2 ((\f a b -> f (toRational a) b) (-) :: Integer -> Rational -> Rational)
    [$p|(a: Double) - (b: Integer)|] =: lift2 ((\f a b -> f a (fromInteger b)) (-) :: Double -> Integer -> Double)
    [$p|(a: Double) - (b: Rational)|] =: lift2 ((\f a b -> f (toRational a) b) (-) :: Double -> Rational -> Rational)
    [$p|(a: Rational) - (b: Integer)|] =: lift2 ((\f a b -> f a (toRational b)) (-) :: Rational -> Integer -> Rational)
    [$p|(a: Rational) - (b: Double)|] =: lift2 ((\f a b -> f a (toRational b)) (-) :: Rational -> Double -> Rational)

    [$p|(a: Integer) * (b: Integer)|] =: lift2 ((*) :: Integer -> Integer -> Integer)
    [$p|(a: Rational) * (b: Rational)|] =: lift2 ((*) :: Rational -> Rational -> Rational)
    [$p|(a: Double) * (b: Double)|] =: lift2 ((*) :: Double -> Double -> Double)
    [$p|(a: Integer) * (b: Double)|] =: lift2 ((\f a b -> f (fromInteger a) b) (*) :: Integer -> Double -> Double)
    [$p|(a: Integer) * (b: Rational)|] =: lift2 ((\f a b -> f (toRational a) b) (*) :: Integer -> Rational -> Rational)
    [$p|(a: Double) * (b: Integer)|] =: lift2 ((\f a b -> f a (fromInteger b)) (*) :: Double -> Integer -> Double)
    [$p|(a: Double) * (b: Rational)|] =: lift2 ((\f a b -> f (toRational a) b) (*) :: Double -> Rational -> Rational)
    [$p|(a: Rational) * (b: Integer)|] =: lift2 ((\f a b -> f a (toRational b)) (*) :: Rational -> Integer -> Rational)
    [$p|(a: Rational) * (b: Double)|] =: lift2 ((\f a b -> f a (toRational b)) (*) :: Rational -> Double -> Rational)

    [$p|(a: Integer) / (b: Integer)|] =: lift2 (div :: Integer -> Integer -> Integer)
    [$p|(a: Rational) / (b: Rational)|] =: lift2 ((/) :: Rational -> Rational -> Rational)
    [$p|(a: Double) / (b: Double)|] =: lift2 ((/) :: Double -> Double -> Double)
    [$p|(a: Integer) / (b: Double)|] =: lift2 ((\f a b -> f (fromInteger a) b) (/) :: Integer -> Double -> Double)
    [$p|(a: Integer) / (b: Rational)|] =: lift2 ((\f a b -> f (toRational a) b) (/) :: Integer -> Rational -> Rational)
    [$p|(a: Double) / (b: Integer)|] =: lift2 ((\f a b -> f a (fromInteger b)) (/) :: Double -> Integer -> Double)
    [$p|(a: Double) / (b: Rational)|] =: lift2 ((\f a b -> f (toRational a) b) (/) :: Double -> Rational -> Rational)
    [$p|(a: Rational) / (b: Integer)|] =: lift2 ((\f a b -> f a (toRational b)) (/) :: Rational -> Integer -> Rational)
    [$p|(a: Rational) / (b: Double)|] =: lift2 ((\f a b -> f a (toRational b)) (/) :: Rational -> Double -> Rational)

    [$p|(a: Integer) ^ (b: Integer)|] =: lift2 ((^) :: Integer -> Integer -> Integer)
    [$p|(a: Double) ^ (b: Double)|] =: lift2 ((**) :: Double -> Double -> Double)
    [$p|(a: Integer) ^ (b: Double)|] =: lift2 ((\f a b -> f (fromInteger a) b) (**) :: Integer -> Double -> Double)
    [$p|(a: Double) ^ (b: Integer)|] =: lift2 ((\f a b -> f a (fromInteger b)) (**) :: Double -> Integer -> Double)
    [$p|(a: Rational) ^ (b: Integer)|] =: lift2 ((^) :: Rational -> Integer -> Rational)

    [$p|(a: Integer) % (b: Integer)|] =: lift2 (mod :: Integer -> Integer -> Integer)
    [$p|(a: Integer) quotient: (b: Integer)|] =: lift2 (quot :: Integer -> Integer -> Integer)
    [$p|(a: Integer) remainder: (b: Integer)|] =: lift2 (rem :: Integer -> Integer -> Integer)
