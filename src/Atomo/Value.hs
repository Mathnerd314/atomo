{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances #-}

module Atomo.Value where

import Control.Concurrent (ThreadId)
import Data.Dynamic
import qualified Data.Text as T
import qualified Data.Vector as V

import Atomo.Types


class Valuable a where
--     | Convert to an Atomo value.
    toValue :: a -> Value

--     | Convert from an Atomo value.
    fromValue :: (Monad m) => Value -> m a
    
--     | Check if an Atomo value is of the same type as the given (ignored) value
    isValue :: a -> Value -> Bool
    isValue _ x = case (fromValue x :: Maybe a) of
            Nothing -> False
            Just y  -> True
            
    valueName :: a -> String

instance Valuable Value where
    toValue = id
    fromValue = return
    isValue _ _ = True
    valueName _ = "Value"

instance Valuable Block where
    toValue = Block
    fromValue (Block i) = return i
    fromValue _ = fail ""
    isValue _ (Block _) = True
    isValue _ _ = False
    valueName _ = "Block"

instance Valuable Bool where
    toValue = Boolean
    fromValue (Boolean c) = return c
    fromValue _ = fail ""
    isValue _ (Boolean _) = True
    isValue _ _ = False
    valueName _ = "Boolean"

instance Valuable Char where
    toValue = Char
    fromValue (Char c) = return c
    fromValue _ = fail ""
    isValue _ (Char _) = True
    isValue _ _ = False
    valueName _ = "Char"

instance Valuable Continuation where
    toValue = Continuation
    fromValue (Continuation c) = return c
    fromValue _ = fail ""
    isValue _ (Continuation _) = True
    isValue _ _ = False
    valueName _ = "Continuation"

instance Valuable Double where
    toValue = Double
    fromValue (Double d) = return d
    fromValue _ = fail ""
    isValue _ (Double _) = True
    isValue _ _ = False
    valueName _ = "Double"

instance Valuable Float where
    toValue = Double . fromRational . toRational
    fromValue (Double d) = return (fromRational . toRational $ d)
    fromValue _ = fail ""
    valueName _ = "Double"

instance Valuable Integer where
    toValue = Integer
    fromValue (Integer i) = return i
    fromValue _ = fail ""
    isValue _ (Integer _) = True
    isValue _ _ = False
    valueName _ = "Integer"

instance Valuable Int where
    toValue = Integer . fromIntegral
    fromValue (Integer i) = return (fromIntegral i)
    fromValue _ = fail ""
    valueName _ = "Integer"

instance Valuable Rational where
    toValue = Rational
    fromValue (Rational i) = return i
    fromValue _ = fail ""
    isValue _ (Rational _) = True
    isValue _ _ = False
    valueName _ = "Rational"

instance Valuable Expr where
    toValue = Expression
    fromValue (Expression i) = return i
    fromValue _ = fail ""
    isValue _ (Expression _) = True
    isValue _ _ = False
    valueName _ = "Expression"

instance Valuable Dynamic where
    toValue = Haskell
    fromValue (Haskell i) = return i
    fromValue _ = fail ""
    isValue _ (Haskell _) = True
    isValue _ _ = False
    valueName _ = "Haskell"

instance Valuable (Message Value) where
    toValue = Message
    fromValue (Message i) = return i
    fromValue _ = fail ""
    isValue _ (Message _) = True
    isValue _ _ = False
    valueName _ = "Message"

instance Valuable Method where
    toValue = Method
    fromValue (Method i) = return i
    fromValue _ = fail ""
    isValue _ (Method _) = True
    isValue _ _ = False
    valueName _ = "Method"

instance Valuable (Particle Value) where
    toValue = Particle
    fromValue (Particle i) = return i
    fromValue _ = fail ""
    isValue _ (Particle _) = True
    isValue _ _ = False
    valueName _ = "Particle"

instance Valuable Pattern where
    toValue = Pattern
    fromValue (Pattern i) = return i
    fromValue _ = fail ""
    isValue _ (Pattern _) = True
    isValue _ _ = False
    valueName _ = "Pattern"

instance Valuable Process where
    toValue = Process
    fromValue (Process i) = return i
    fromValue _ = fail ""
    isValue _ (Process _) = True
    isValue _ _ = False
    valueName _ = "Process"

instance Valuable ORef where
    toValue = Reference
    fromValue (Reference i) = return i
    fromValue _ = fail ""
    isValue _ (Reference _) = True
    isValue _ _ = False
    valueName _ = "Reference"

-- TODO: find magic GHC invocation to make these play nice w/ VVector

-- instance Valuable a => Valuable [a] where
--     toValue xs = list (map toValue xs)
--     fromValue (List v) = mapM fromValue (V.toList v)
--     fromValue _ = fail ""
--     valueName _ = "List"

-- instance Valuable a => Valuable (V.Vector a) where
--     toValue xs = List (V.map toValue xs)
--     fromValue (List v) = V.mapM fromValue v
--     fromValue _ = fail ""
--     valueName _ = "List"

instance Valuable (VVector) where
    toValue xs = List xs
    fromValue (List v) = return v
    fromValue _ = fail ""
    isValue _ (List _) = True
    isValue _ _ = False
    valueName _ = "List"

instance Valuable T.Text where
    toValue = String
    fromValue (String s) = return s
    fromValue _ = fail ""
    isValue _ (String _) = True
    isValue _ _ = False
    valueName _ = "String"
