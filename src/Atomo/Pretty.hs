{-# LANGUAGE TypeSynonymInstances #-}
module Atomo.Pretty (Pretty(..), prettyStack) where

import Data.IORef
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Ratio
import Text.PrettyPrint hiding (braces)
import System.IO.Unsafe
import qualified Data.Vector as V
import qualified Language.Haskell.Interpreter as H

import Atomo.Method
import Atomo.Types hiding (keyword)
import Atomo.Parser.Base (opLetters)


data Context
    = CNone
    | CDefine
    | CKeyword
    | CSingle
    | CArgs
    | CPattern
    | CList

class Pretty a where
    pretty :: a -> Doc
    prettyFrom :: Context -> a -> Doc

    pretty = prettyFrom CNone


instance Pretty Value where
    prettyFrom _ (Block _ ps es)
        | null ps = braces exprs
        | otherwise = braces $ sep (map (prettyFrom CArgs) ps) <+> char '|' <+> exprs
      where
        exprs = sep . punctuate (text ";") $ map pretty es
    prettyFrom _ (Boolean b) = text $ show b
    prettyFrom _ (Char c) = text $ show c
    prettyFrom _ (Continuation _) = internal "continuation" empty
    prettyFrom _ (Double d) = double d
    prettyFrom _ (Expression e) = internal "expression" $ pretty e
    prettyFrom _ (Haskell v) = internal "haskell" $ text (show v)
    prettyFrom _ (Integer i) = integer i
    prettyFrom _ (List l) =
        brackets . hsep . punctuate comma $ map (prettyFrom CList) vs
      where vs = V.toList (unsafePerformIO (readIORef l))
    prettyFrom _ (Message m) = internal "message" $ pretty m
    prettyFrom _ (Method (Slot p _)) = internal "slot" $ parens (pretty p)
    prettyFrom _ (Method (Responder p _ _)) = internal "responder" $ parens (pretty p)
    prettyFrom _ (Particle p) = char '@' <> pretty p
    prettyFrom _ (Pattern p) = internal "pattern" $ pretty p
    prettyFrom _ (Process _ tid) =
        internal "process" $ text (words (show tid) !! 1)
    prettyFrom CNone (Reference r) = pretty (unsafePerformIO (readIORef r))
    prettyFrom _ (Rational r) = integer (numerator r) <> char '/' <> integer (denominator r)
    prettyFrom _ (Reference _) = internal "object" empty
    prettyFrom _ (String s) = text (show s)

instance Pretty Object where
    prettyFrom _ (Object ds (ss, ks)) = vcat
        [ internal "object" $ parens (text "delegates to" <+> pretty ds)

        , if not (nullMap ss)
              then nest 2 $ vcat (flip map (elemsMap ss) $ (\ms ->
                  vcat (map prettyMethod ms))) <>
                      if not (nullMap ks)
                          then char '\n'
                          else empty
              else empty

        , if not (nullMap ks)
              then nest 2 . vcat $ flip map (elemsMap ks) $ \ms ->
                  vcat (map prettyMethod ms) <> char '\n'
              else empty
        ]
      where
        prettyMethod (Slot { mPattern = p, mValue = v }) =
            prettyFrom CDefine p <+> text ":=" <++> prettyFrom CDefine v
        prettyMethod (Responder { mPattern = p, mExpr = e }) =
            prettyFrom CDefine p <+> text ":=" <++> prettyFrom CDefine e

instance Pretty Message where
    prettyFrom _ (Single _ n t) = prettyFrom CSingle t <+> text n
    prettyFrom _ (Keyword _ ns vs) = keywords ns vs


instance Pretty Particle where
    prettyFrom _ (PMSingle e) = text e
    prettyFrom _ (PMKeyword ns vs)
        | all (not . isJust) vs = text . concat $ map keyword ns
        | not (isJust (head vs)) =
            parens $ headlessKeywords' prettyVal ns (tail vs)
        | otherwise = parens (keywords' prettyVal ns vs)
      where
        prettyVal me =
            case me of
                Nothing -> text "_"
                Just e -> prettyFrom CKeyword e


instance Pretty Pattern where
    prettyFrom _ PAny = text "_"
    prettyFrom _ (PHeadTail h t) =
        parens $ pretty h <+> text "." <+> pretty t
    prettyFrom _ (PKeyword _ ns (PObject ETop {}:vs)) =
        headlessKeywords ns vs
    prettyFrom _ (PKeyword _ ns (PThis:vs)) =
        headlessKeywords ns vs
    prettyFrom _ (PKeyword _ ns vs) = keywords ns vs
    prettyFrom _ (PList ps) =
        brackets . sep $ punctuate comma (map pretty ps)
    prettyFrom _ (PMatch v) = prettyFrom CPattern v
    prettyFrom _ (PNamed n PAny) = text n
    prettyFrom _ (PNamed n p) = parens $ text n <> colon <+> pretty p
    prettyFrom _ (PObject e) = pretty e
    prettyFrom _ (PPMKeyword ns ps)
        | all isAny ps = char '@' <> text (concat $ map keyword ns)
        | isAny (head ps) =
            char '@' <> parens (headlessKeywords' (prettyFrom CKeyword) ns (tail ps))
        | otherwise = char '@' <> parens (keywords' (prettyFrom CKeyword) ns ps)
      where
        isAny PAny = True
        isAny _ = False
    prettyFrom _ (PSingle _ n (PObject ETop {})) = text n
    prettyFrom _ (PSingle _ n PThis) = text n
    prettyFrom _ (PSingle _ n p) = pretty p <+> text n
    prettyFrom _ PThis = text "<this>"


instance Pretty Expr where
    prettyFrom _ (Define _ p v) = prettyFrom CDefine p <+> text ":=" <++> prettyFrom CDefine v
    prettyFrom _ (Set _ p v)    = prettyFrom CDefine p <+> text "=" <++> prettyFrom CDefine v
    prettyFrom CKeyword (Dispatch _ m@(EKeyword {})) = parens $ pretty m
    prettyFrom CSingle (Dispatch _ m@(EKeyword {})) = parens $ pretty m
    prettyFrom c (Dispatch _ m) = prettyFrom c m
    prettyFrom _ (Operator _ ns a i) =
        text "operator" <+> assoc a <+> integer i <+> sep (punctuate comma (map text ns))
      where
        assoc ALeft = text "left"
        assoc ARight = text "right"
    prettyFrom c (Primitive _ v) = prettyFrom c v
    prettyFrom _ (EBlock _ ps es)
        | null ps = braces exprs
        | otherwise = braces $ sep (map pretty ps) <+> char '|' <+> exprs
      where
        exprs = sep . punctuate (text ";") $ map pretty es
    prettyFrom _ (EDispatchObject {}) = text "dispatch"
    prettyFrom CDefine (EVM {}) = text "..."
    prettyFrom _ (EVM {}) = text "<vm>"
    prettyFrom _ (EList _ es) =
        brackets . sep . punctuate comma $ map (prettyFrom CList) es
    prettyFrom c (EParticle _ p) = char '@' <> prettyFrom c p
    prettyFrom _ (ETop {}) = text "<top>"


instance Pretty EMessage where
    prettyFrom _ (ESingle _ n (ETop {})) = text n
    prettyFrom _ (ESingle _ n t) = prettyFrom CSingle t <+> text n
    prettyFrom _ (EKeyword _ ns (ETop {}:es)) = headlessKeywords ns es
    prettyFrom _ (EKeyword _ ns es) = keywords ns es


instance Pretty EParticle where
    prettyFrom _ (EPMSingle e) = text e
    prettyFrom _ (EPMKeyword ns es)
        | all (not . isJust) es = text . concat $ map keyword ns
        | not (isJust (head es)) =
            parens $ headlessKeywords' prettyVal ns (tail es)
        | otherwise = parens $ keywords' prettyVal ns es
      where
        prettyVal me =
            case me of
                Nothing -> text "_"
                Just e -> pretty e


instance Pretty AtomoError where
    prettyFrom _ (Error v) = text "error:" <+> pretty v
    prettyFrom _ (DidNotUnderstand m) =
        text "message not understood:" $$ nest 2 (pretty m)
    prettyFrom _ (ParseError e) =
        text "parse error:" $$ nest 2 (text (show e))
    prettyFrom _ (Mismatch p v) =
        text "pattern" <+> char '<' <> pretty p <> char '>' <+> text "did not match value:" <+> pretty v
    prettyFrom _ (ImportError (H.UnknownError s)) =
        text "import error:" <+> text s
    prettyFrom _ (ImportError (H.WontCompile ges)) =
        text "import error:" $$ nest 2 (vcat (map (\e -> text e <> char '\n') (nub (map H.errMsg ges))))
    prettyFrom _ (ImportError (H.NotAllowed s)) =
        text "import error:" <+> text s
    prettyFrom _ (ImportError (H.GhcException s)) =
        text "import error:" <+> text s
    prettyFrom _ (FileNotFound fn) =
        text "file not found:" <+> text fn
    prettyFrom _ (ParticleArity e g) =
        text . unwords $
            [ "particle needs"
            , show e
            , "values to complete,"
            , show g
            , "given"
            ]
    prettyFrom _ (BlockArity e g) =
        text . unwords $
            [ "block expects"
            , show e
            , "arguments,"
            , show g
            , "given"
            ]
    prettyFrom _ NoExpressions = text "no expressions to evaluate"
    prettyFrom _ (ValueNotFound d v) =
        text ("could not find a " ++ d ++ " in") <+> pretty v
    prettyFrom _ (DynamicNeeded t) =
        text "dynamic value not of type" <+> text t


instance Pretty Delegates where
    prettyFrom _ [] = internal "bottom" empty
    prettyFrom _ [_] = text "1 object"
    prettyFrom _ ds = text $ show (length ds) ++ " objects"



prettyStack :: Expr -> Doc
prettyStack (EVM {}) = text "... internal ..."
prettyStack e =
    case eLocation e of
        Nothing -> text "(...)" $$ nest 2 (pretty e)
        Just s -> text (show s) $$ nest 2 (pretty e)

internal :: String -> Doc -> Doc
internal n d = char '<' <> text n <+> d <> char '>'

braces :: Doc -> Doc
braces d = char '{' <+> d <+> char '}'

headlessKeywords' :: (a -> Doc) -> [String] -> [a] -> Doc
headlessKeywords' p (k:ks) (v:vs) =
    text (keyword k) <+> p v <++> headlessKeywords'' p ks vs
headlessKeywords' _ _ _ = empty

headlessKeywords'' :: (a -> Doc) -> [String] -> [a] -> Doc
headlessKeywords'' p (k:ks) (v:vs) =
    text (keyword k) <+> p v <+++> headlessKeywords'' p ks vs
headlessKeywords'' _ _ _ = empty

keywords' :: (a -> Doc) -> [String] -> [a] -> Doc
keywords' p ks (v:vs) =
    p v <+> headlessKeywords' p ks vs
keywords' _ _ _ = empty

headlessKeywords :: Pretty a => [String] -> [a] -> Doc
headlessKeywords = headlessKeywords' (prettyFrom CKeyword)

keywords :: Pretty a => [String] -> [a] -> Doc
keywords = keywords' (prettyFrom CKeyword)

keyword :: String -> String
keyword k
    | all (`elem` opLetters) k = k
    | otherwise                = k ++ ":"

infixr 4 <++>, <+++>

-- similar to <+>, but the second half will be nested to prevent long lines
(<++>) :: Doc -> Doc -> Doc
(<++>) a b
    | length (show a ++ show b) > 80 = a $$ nest 2 b
    | otherwise = a <+> b

-- similar to <++>, but without nesting
(<+++>) :: Doc -> Doc -> Doc
(<+++>) a b
    | length (show a ++ show b) > 80 = a $$ b
    | otherwise = a <+> b
