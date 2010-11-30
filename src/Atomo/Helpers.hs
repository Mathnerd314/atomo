{-# LANGUAGE ScopedTypeVariables #-}
module Atomo.Helpers where

import Control.Monad.State
import Data.Dynamic
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Environment
import Atomo.Method
import Atomo.Pattern
import Atomo.Types
import Atomo.Value


infixr 0 =:, =::

-- | Define a method as an action returning a value.
(=:) :: Message Pattern -> VM Value -> VM ()
pat =: vm = define pat (EVM Nothing Nothing vm)

-- | Set a slot to a given value.
(=::) :: Message Pattern -> Value -> VM ()
pat =:: v = define pat (Primitive Nothing v)

-- | Define a method that evaluates e.
(=:::) :: Message Pattern -> Expr -> VM ()
pat =::: e = define pat e

-- | Find a value, searching through an object's delegates, and throwing
-- @\@could-not-find:in:@ if it is not found.
findValue :: String -> (Value -> Bool) -> Value -> VM Value
findValue _ t v | t v = return v
findValue d t v = findValue' t v >>= maybe die return
  where
    die = throwError (ValueNotFound d v)

-- | Same as `findValue', but returning Nothing instead of failing
findValue' :: (Value -> Bool) -> Value -> VM (Maybe Value)
findValue' t v | t v = return (Just v)
findValue' t (Reference r) = do
    o <- liftIO (readIORef r)
    findDels (oDelegates o)
  where
    findDels [] = return Nothing
    findDels (d:ds) = do
        f <- findValue' t d
        case f of
            Nothing -> findDels ds
            Just v -> return (Just v)
findValue' _ _ = return Nothing

-- | `findValue' for specific types
findV :: (Valuable a) => a -> Value -> VM Value
findV a v
    | isValue a v = return v
    | otherwise = findValue (valueName a) (isValue a) v

getV :: forall a. (Valuable a) => Value -> VM a
{-# SPECIALISE getV :: Value -> VM Bool #-}
{-# SPECIALISE getV :: Value -> VM Char #-}
{-# SPECIALISE getV :: Value -> VM Double #-}
{-# SPECIALISE getV :: Value -> VM Integer #-}
{-# SPECIALISE getV :: Value -> VM Rational #-}
{-# SPECIALISE getV :: Value -> VM T.Text #-}
{-# SPECIALISE getV :: Value -> VM Value #-}
{-# SPECIALISE getV :: Value -> VM VVector #-}
getV x = findV (undefined :: a) x >>= fromValue

-- | Find a String given an expression to evaluate.
getString :: Expr -> VM String
getString e = eval e >>= liftM (fromText . fromString) . (findV (undefined :: T.Text))

-- | Find a Data.Text.Text given an expression to evaluate.
getText :: Expr -> VM T.Text
getText e = eval e >>= (findV (undefined :: T.Text)) >>= fromValue

-- | Find a list of values, given an expression to evaluate.
getList :: Expr -> VM [Value]
getList = liftM V.toList . getVector

-- | Find a VVector, given an expression to evaluate.
getVector :: Expr -> VM VVector
getVector e = eval e
    >>= (findV (undefined :: VVector))
    >>= fromValue

-- | Dispatch a single message to the current toplevel.
here :: String -> VM Value
here n = gets top >>= dispatch . single n

lift1 :: (Valuable a, Valuable b) => (a -> b) -> VM Value
{-# SPECIALIZE lift1 :: (Double -> Double) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Double -> Integer) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Integer -> Double) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Integer -> Rational) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Rational -> Double) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Rational -> Integer) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Rational -> Rational) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Char -> Integer) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Char -> Bool) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Char -> Value) -> VM Value #-}
{-# SPECIALIZE lift1 :: (Integer -> Char) -> VM Value #-}
lift1 f = do
    a <- here "a" >>= getV
    return (toValue (f a))

lift2 :: (Valuable a, Valuable b, Valuable c) => (a -> b -> c) -> VM Value
{-# SPECIALIZE lift2 :: (Double -> Double -> Double) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Double -> Double -> Rational) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Double -> Integer -> Double) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Double -> Rational -> Rational) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Integer -> Double -> Double) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Integer -> Integer -> Integer) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Integer -> Rational -> Rational) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Rational -> Double -> Rational) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Rational -> Integer -> Rational) -> VM Value #-}
{-# SPECIALIZE lift2 :: (Rational -> Rational -> Rational) -> VM Value #-}
lift2 f = do
    a <- here "a" >>= getV
    b <- here "b" >>= getV
    return (toValue (f a b))

-- | if-then-else based on a VM action yielding a Boolean Value.
ifVM :: VM Value -> VM a -> VM a -> VM a
ifVM c a b = do
    r <- c
    if r == Boolean True then a else b

-- | if-then-else based on a VM action yielding a Bool.
ifVM' :: VM Bool -> VM a -> VM a -> VM a
ifVM' c a b = do
    r <- c
    if r then a else b

-- | if-then-else based on an expression to evaluate.
ifE :: Expr -> VM a -> VM a -> VM a
ifE = ifVM . eval

-- | Get a value's object.
referenceTo :: Value -> VM Value
{-# INLINE referenceTo #-}
referenceTo = liftM Reference . orefFor

-- | Call a block with the given arguments. Creates a scope, checks that its
-- argument patterns match, and executes it with the bindings.
callBlock :: Block -> [Value] -> VM Value
callBlock (s, ps, es) vs = do
    is <- gets primitives
    checkArgs is ps vs
    doBlock (toMethods . concat $ zipWith bindings' ps vs) s es
  where
    checkArgs _ [] _ = return (particle "ok")
    checkArgs _ _ [] = throwError (BlockArity (length ps) (length vs))
    checkArgs is (p:ps') (v:vs')
        | match is Nothing p v = checkArgs is ps' vs'
        | otherwise = throwError (Mismatch p v)

-- | Evaluate multiple expressions given a context and bindings for the
-- toplevel object.
doBlock :: MethodMap -> Value -> [Expr] -> VM Value
{-# INLINE doBlock #-}
doBlock bms s es = do
    blockScope <- newObject $ \o -> o
        { oDelegates = [s]
        , oMethods = (bms, emptyMap)
        }

    withTop blockScope (evalAll es)

-- | Get the object backing a value.
objectFor :: Value -> VM Object
{-# INLINE objectFor #-}
objectFor v = orefFor v >>= liftIO . readIORef

-- | Does one value delegate to another?
delegatesTo :: Value -> Value -> VM Bool
delegatesTo f t = do
    o <- objectFor f
    delegatesTo' (oDelegates o)
  where
    delegatesTo' [] = return False
    delegatesTo' (d:ds)
        | t `elem` (d:ds) = return True
        | otherwise = do
            o <- objectFor d
            delegatesTo' (oDelegates o ++ ds)

-- | Is one value an instance of, equal to, or a delegation to another?
--
-- For example, 1 is-a?: Integer, but 1 does not delegates-to?: Integer
isA :: Value -> Value -> VM Bool
isA x y = do
    xr <- orefFor x
    yr <- orefFor y

    if xr == yr
        then return True
        else do
            ds <- liftM oDelegates (objectFor x)
            isA' ds
  where
    isA' [] = return False
    isA' (d:ds) = do
        di <- isA d y
        if di
            then return True
            else isA' ds

-- | Attempt conversion from a Haskell Value, given the type we expect as
-- a string.
--
-- If conversion fails, raises @\@dynamic-needed:@ with the given string.
fromHaskell :: forall a. Typeable a => Value -> VM a
fromHaskell (Haskell d) =
    case fromDynamic d of
        Just a -> return a
        Nothing ->
            raise ["dynamic-needed", "got"]
                [ string (show (typeOf (undefined :: a)))
                , string (show (dynTypeRep d))
                ]
fromHaskell u =
    raise ["dynamic-needed", "given"]
        [string (show (typeOf (undefined :: a))), u]

-- | Convert an Atomo Haskell dynamic value into its value, erroring on
-- failure.
fromHaskell' :: forall a. Typeable a => Value -> a
fromHaskell' (Haskell d) =
    case fromDynamic d of
        Just a -> a
        Nothing ->
            error $ unwords
                [ "needed Haskell value of type"
                , show (typeOf (undefined :: a))
                , "but got"
                , show (dynTypeRep d)
                ]
fromHaskell' v =
    error $ unwords
        [ "needed Haskell value of type"
        , show (typeOf (undefined :: a))
        , "but given value"
        , show v
        ]

-- | `toPattern', raising @\@unknown-pattern:@ if conversion fails.
toPattern' :: Expr -> VM Pattern
toPattern' = tryPattern toPattern

-- | `toDefinePattern', raising @\@unknown-pattern:@ if conversion fails.
toDefinePattern' :: Expr -> VM (Message Pattern)
toDefinePattern' = tryPattern toDefinePattern

-- | `toRolePattern', raising @\@unknown-pattern:@ if conversion fails.
toRolePattern' :: Expr -> VM Pattern
toRolePattern' = tryPattern toRolePattern

-- | `toMacroPattern', raising @\@unknown-pattern:@ if conversion fails.
toMacroPattern' :: Expr -> VM (Message Pattern)
toMacroPattern' = tryPattern toMacroPattern

-- | Try a given pattern conversion, raising @\@unknown-pattern:@ if conversion
-- fails.
tryPattern :: (Expr -> Maybe p) -> Expr -> VM p
tryPattern c e = 
    case c e of
        Nothing -> raise ["unknown-pattern"] [Expression e]
        Just p -> return p

-- | Fill in the empty values of a particle. The number of values missing
-- is expected to be equal to the number of values provided.
completeKP :: [Maybe Value] -> [Value] -> [Value]
completeKP [] _ = []
completeKP (Nothing:mvs') (v:vs') = v : completeKP mvs' vs'
completeKP (Just v:mvs') vs' = v : completeKP mvs' vs'
completeKP mvs' vs' = error $ "impossible: completeKP on " ++ show (mvs', vs')
