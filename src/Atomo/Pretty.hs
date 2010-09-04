module Atomo.Pretty (Pretty(..)) where

import Data.IORef
import Data.Maybe (isJust)
import Text.PrettyPrint hiding (braces)
import System.IO.Unsafe
import qualified Data.Vector as V

import Atomo.Types
import Atomo.Parser.Base (opLetters)


data Context
    = CNone
    | CKeyword
    | CSingle
    | CArgs

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
    prettyFrom _ (Char c) = text $ show c
    prettyFrom _ (Double d) = double d
    prettyFrom _ (Expression e) = internal "expression" $ pretty e
    prettyFrom _ (Haskell v) = internal "haskell" $ text (show v)
    prettyFrom _ (Integer i) = integer i
    prettyFrom _ (List l)
        | not (null vs) && all isChar vs = text $ show (map (\(Char c) -> c) vs)
        | otherwise = brackets . hsep . punctuate comma $ map pretty vs
      where vs = V.toList (unsafePerformIO (readIORef l))
    prettyFrom _ (Message m) = internal "message" $ pretty m
    prettyFrom _ (Particle p) = char '@' <> pretty p
    prettyFrom _ (Pattern p) = internal "pattern" $ pretty p
    prettyFrom _ (Process _ tid) =
        internal "process" $ text (words (show tid) !! 1)
    prettyFrom _ (Reference _) = internal "reference" empty


instance Pretty Message where
    prettyFrom _ (Single _ n t) = pretty t <+> text n
    prettyFrom _ (Keyword _ ns vs) = keywords ns vs


instance Pretty Particle where
    prettyFrom _ (PMSingle e) = text e
    prettyFrom _ (PMKeyword ns vs)
        | all (== Nothing) vs = text . concat $ map keyword ns
        | head vs == Nothing =
            parens $ headlessKeywords' prettyVal ns (tail vs)
        | otherwise = parens (keywords' prettyVal ns vs)
      where
        prettyVal me =
            case me of
                Nothing -> text "_"
                Just e -> pretty e


instance Pretty Pattern where
    prettyFrom _ PAny = text "_"
    prettyFrom _ (PHeadTail h t) =
        parens $ pretty h <+> text "." <+> pretty t
    prettyFrom _ (PKeyword _ ns (PSelf:vs)) =
        headlessKeywords ns vs
    prettyFrom _ (PKeyword _ ns vs) = keywords ns vs
    prettyFrom _ (PList ps) = brackets . sep $ punctuate comma (map pretty ps)
    prettyFrom _ (PMatch v) = pretty v
    prettyFrom _ (PNamed n PAny) = text n
    prettyFrom _ (PNamed n p) = parens $ text n <> colon <+> pretty p
    prettyFrom _ (PObject e) = pretty e
    prettyFrom _ PSelf = text "<self>"
    prettyFrom _ (PSingle _ n PSelf) = text n
    prettyFrom _ (PSingle _ n p) = pretty p <+> text n


instance Pretty Expr where
    prettyFrom _ (Define _ p v) = pretty p <+> text ":=" <+> pretty v
    prettyFrom _ (Set _ p v)    = pretty p <+> text "=" <+> pretty v
    prettyFrom CKeyword (Dispatch _ m@(EKeyword {})) = parens $ pretty m
    prettyFrom _ (Dispatch _ m) = pretty m
    prettyFrom _ (Primitive _ v) = pretty v
    prettyFrom _ (EBlock _ ps es)
        | null ps = braces exprs
        | otherwise = braces $ sep (map pretty ps) <+> char '|' <+> exprs
      where
        exprs = sep . punctuate (text ";") $ map pretty es
    prettyFrom _ (EDispatchObject {}) = text "dispatch"
    prettyFrom _ (EVM {}) = text "<vm>"
    prettyFrom _ (EList _ es)
        | all isPrimChar es = text $ show (map (\(Primitive _ (Char c)) -> c) es)
        | otherwise = brackets . sep . punctuate comma $ map pretty es
      where
        isPrimChar (Primitive _ (Char _)) = True
        isPrimChar _ = False
    prettyFrom _ (EParticle _ p) = char '@' <> pretty p
    prettyFrom _ (ETop {}) = text "<top>"


instance Pretty EMessage where
    prettyFrom _ (ESingle _ n (ETop {})) = text n
    prettyFrom _ (ESingle _ n t) = pretty t <+> text n
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
    pretty (ErrorMsg msg) = text msg
    pretty (DidNotUnderstand m) =
        text "message not understood:" <+> pretty m
    pretty (ParseError e) =
        text "parse error:" <+> text (show e)
    pretty (Mismatch p v) =
        text "pattern" <+> char '<' <> pretty p <> char '>' <+> text "did not match value:" <+> pretty v
    {-pretty (ImportError (H.UnknownError s)) =-}
        {-text "import error:" <+> text s-}
    {-pretty (ImportError (H.WontCompile ges)) =-}
        {-text "import error:" <+> sep (map text (map H.errMsg ges))-}
    {-pretty (ImportError (H.NotAllowed s)) =-}
        {-text "import error:" <+> text s-}
    {-pretty (ImportError (H.GhcException s)) =-}
        {-text "import error:" <+> text s-}


internal :: String -> Doc -> Doc
internal n d = char '<' <> text n <+> d <> char '>'

braces :: Doc -> Doc
braces d = char '{' <+> d <+> char '}'

headlessKeywords' :: (a -> Doc) -> [String] -> [a] -> Doc
headlessKeywords' p (k:ks) (v:vs) =
    text (keyword k) <+> p v <+> headlessKeywords' p ks vs
headlessKeywords' _ _ _ = empty

keywords' :: (a -> Doc) -> [String] -> [a] -> Doc
keywords' p ks (v:vs) =
    p v <+> headlessKeywords' p ks vs
keywords' p _ _ = empty

headlessKeywords :: Pretty a => [String] -> [a] -> Doc
headlessKeywords = headlessKeywords' (prettyFrom CKeyword)

keywords :: Pretty a => [String] -> [a] -> Doc
keywords = keywords' (prettyFrom CKeyword)

keyword :: String -> String
keyword k
    | all (`elem` opLetters) k = k
    | otherwise                = k ++ ":"

