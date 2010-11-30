{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}
module Atomo.Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
import Control.Monad.Cont
import Control.Monad.State
import Data.Dynamic
import Data.Hashable (hash)
import Data.List (nub)
import Data.IORef
import Text.Parsec (ParseError, SourcePos)
import Text.PrettyPrint (Doc)
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.Haskell.Interpreter as H
import qualified Language.Haskell.TH.Syntax as S


-- | The Atomo VM. A Continuation monad wrapping a State monad.
type VM = ContT Value (StateT Env IO)

-- | All values usable in Atomo.
data Value
    -- | A block of expressions, bound to a context and with optional arguments.
    = Block !Block

    -- | A boolean value.
    | Boolean { fromBoolean :: !Bool }

    -- | A character value.
    | Char { fromChar :: {-# UNPACK #-} !Char }

    -- | A continuation reference.
    | Continuation { fromContinuation :: Continuation }

    -- | A double value.
    | Double { fromDouble :: {-# UNPACK #-} !Double }

    -- | An expression value.
    | Expression { fromExpression :: Expr }

    -- | A dynamically-typed Haskell value.
    | Haskell Dynamic

    -- | An Integer value.
    | Integer !Integer

    -- | A vector of Values.
    | List VVector

    -- | A message value.
    | Message { fromMessage :: Message Value }

    -- | A method value.
    | Method { fromMethod :: Method }

    -- | A particle value.
    | Particle { fromParticle :: Particle Value }

    -- | A process; a communications channel and the thread's ID.
    | Process !Process

    -- | A pattern value.
    | Pattern { fromPattern :: Pattern }

    -- | A rational value.
    | Rational Rational

    -- | An object reference.
    | Reference
        { rORef :: {-# UNPACK #-} !ORef
        }

    -- | A string value; Data.Text.Text.
    | String { fromString :: !T.Text }
    deriving (Show, Typeable)

-- | A pure object.
data Object =
    Object
        { -- | The object's delegates list.
          oDelegates :: !Delegates

          -- | A pair of (single, keyword) methods.
        , oMethods :: !(MethodMap, MethodMap)
        }
    deriving (Show, Typeable)

-- | Methods, slot, and macro methods.
data Method
    -- | Responds to a message by evaluating an expression in the given context.
    = Responder
        { mPattern :: !(Message Pattern)
        , mContext :: !Value
        , mExpr :: !Expr
        }

    -- | Responds to a macro message by evaluating an expression.
    | Macro
        { mPattern :: !(Message Pattern)
        , mExpr :: !Expr
        }

    -- | Responds to a message by returning a value.
    | Slot
        { mPattern :: !(Message Pattern)
        , mValue :: !Value
        }
    deriving (Eq, Show, Typeable)

-- | Messages sent to objects.
data Message v
    -- | A keyword-delimited message with multiple targets.
    = Keyword
        { mID :: !Int
        , mNames :: [String]
        , mTargets :: [v]
        }

    -- | A single message sent to one target.
    | Single
        { mID :: !Int
        , mName :: String
        , mTarget :: v
        }
    deriving (Eq, Show, Typeable)

-- | Partial messages.
data Particle v
    -- | A single message with no target.
    = PMSingle String

    -- | A keyword message with many optional targets.
    | PMKeyword [String] [Maybe v]
    deriving (Eq, Show, Typeable)

-- | Shortcut error values.
data AtomoError
    = Error Value
    | ParseError ParseError
    | DidNotUnderstand (Message Value)
    | Mismatch Pattern Value
    | ImportError H.InterpreterError
    | FileNotFound String
    | ParticleArity Int Int
    | BlockArity Int Int
    | NoExpressions
    | ValueNotFound String Value
    | DynamicNeeded String
    deriving (Show, Typeable)

-- | Pattern-matching.
--
-- The Eq instance only checks equivalence. For example, named pattern matches
-- only match their patterns, not the names.
data Pattern
    = PAny
    | PHeadTail Pattern Pattern
    | PList [Pattern]
    | PMatch Value
    | PMessage (Message Pattern)
    | PInstance Pattern
    | PStrict Pattern
    | PNamed String Pattern
    | PObject Expr
    | PPMKeyword [String] [Pattern]
    | PThis

    -- expression types, used in macros
    | PEDispatch
    | PEOperator
    | PEPrimitive
    | PEBlock
    | PEList
    | PEMacro
    | PEParticle
    | PETop
    | PEQuote
    | PEUnquote

    | PExpr Expr
    deriving (Show, Typeable)

-- | Expressions; the nodes in a syntax tree.
data Expr
    = Define
        { eLocation :: Maybe SourcePos
        , emPattern :: Message Pattern
        , eExpr :: Expr
        }
    | Set
        { eLocation :: Maybe SourcePos
        , ePattern :: Pattern
        , eExpr :: Expr
        }
    | Dispatch
        { eLocation :: Maybe SourcePos
        , eMessage :: Message Expr
        }
    | Operator
        { eLocation :: Maybe SourcePos
        , eNames :: [String]
        , eAssoc :: Assoc
        , ePrec :: Integer
        }
    | Primitive
        { eLocation :: Maybe SourcePos
        , eValue :: !Value
        }
    | EBlock
        { eLocation :: Maybe SourcePos
        , eArguments :: [Pattern]
        , eContents :: [Expr]
        }
    | EList
        { eLocation :: Maybe SourcePos
        , eContents :: [Expr]
        }
    | EMacro
        { eLocation :: Maybe SourcePos
        , emPattern :: Message Pattern
        , eExpr :: Expr
        }
    | EForMacro
        { eLocation :: Maybe SourcePos
        , eExpr :: Expr
        }
    | EParticle
        { eLocation :: Maybe SourcePos
        , eParticle :: Particle Expr
        }
    | ETop
        { eLocation :: Maybe SourcePos
        }
    | EVM
        { eLocation :: Maybe SourcePos
        , ePretty :: Maybe Doc
        , eAction :: VM Value
        }
    | EQuote
        { eLocation :: Maybe SourcePos
        , eExpr :: Expr
        }
    | EUnquote
        { eLocation :: Maybe SourcePos
        , eExpr :: Expr
        }
    deriving (Show, Typeable)

-- | Atomo's VM state.
data Env =
    Env
        { -- | The current toplevel object.
          top :: Value

          -- | A record of objects representing the primitive values.
        , primitives :: IDs

          -- | This process's communications channel.
        , channel :: Channel

          -- | Function to call which will shut down the entire VM when called
          -- from any thread.
        , halt :: IO ()

          -- | Paths to search for files.
        , loadPath :: [FilePath]

          -- | Files aready loaded.
        , loaded :: [FilePath]

          -- | The last N expressions evaluated up to the current error.
        , stack :: [Expr]

          -- | The parser's state, passed around when a parser action needs to
          -- be run.
        , parserState :: ParserState
        }
    deriving Typeable

-- | Operator associativity.
data Assoc = ALeft | ARight
    deriving (Eq, Show, Typeable)

-- | A giant record of the objects for each primitive value.
data IDs =
    IDs
        { idObject :: ORef
        , idBlock :: ORef
        , idBoolean :: ORef
        , idChar :: ORef
        , idContinuation :: ORef
        , idDouble :: ORef
        , idExpression :: ORef
        , idHaskell :: ORef
        , idInteger :: ORef
        , idList :: ORef
        , idMessage :: ORef
        , idMethod :: ORef
        , idParticle :: ORef
        , idProcess :: ORef
        , idPattern :: ORef
        , idRational :: ORef
        , idString :: ORef
        }
    deriving (Show, Typeable)

-- | State underlying the parser, and saved in the VM.
data ParserState =
    ParserState
        { -- | Operator precedence and associativity.
          psOperators :: Operators

          -- | All defined macros; (single macros, keyword macros).
        , psMacros :: (MethodMap, MethodMap)

          -- | Are we parsing in a quasiquote?
        , psInQuote :: Bool

          -- | The number of macros we've expanded.
        , psClock :: Integer

          -- | Environment that for-macro and macro methods are evaluated in.
        , psEnvironment :: Value
        }
    deriving (Show, Typeable)

-- | A map of operators to their associativity and precedence.
type Operators = [(String, (Assoc, Integer))]

-- | The list of values an object delegates to.
type Delegates = [Value]

-- | A channel for sending values through to processes.
type Channel = Chan Value

-- | A selector-indexed and precision-sorted map of methods.
type MethodMap = M.IntMap [Method]

-- | A reference to an object.
type ORef = IORef Object

-- | A vector containing values.
type VVector = V.Vector Value

-- | A reference to a continuation function.
type Continuation = IORef (Value -> VM Value)

-- | A block of expressions, bound to a context and with optional arguments.
type Block = (Value, [Pattern], [Expr])

-- | A process; a communications channel and the thread's ID.
type Process = (Channel, ThreadId)

instance Eq Value where
    (==) (Block (at, aps, aes)) (Block (bt, bps, bes)) =
        at == bt && aps == bps && aes == bes
    (==) (Boolean a) (Boolean b) = a == b
    (==) (Char a) (Char b) = a == b
    (==) (Continuation a) (Continuation b) = a == b
    (==) (Double a) (Double b) = a == b
    (==) (Expression a) (Expression b) = a == b
    (==) (Haskell _) (Haskell _) = False
    (==) (Integer a) (Integer b) = a == b
    (==) (List a) (List b) = a == b
    (==) (Message a) (Message b) = a == b
    (==) (Method a) (Method b) = a == b
    (==) (Particle a) (Particle b) = a == b
    (==) (Process (_, a)) (Process (_, b)) = a == b
    (==) (Rational a) (Rational b) = a == b
    (==) (Reference a) (Reference b) = a == b
    (==) (String a) (String b) = a == b
    (==) _ _ = False

instance Eq Pattern where
    -- check if two patterns are "equivalent", ignoring names for patterns
    -- and other things that mean the same thing
    (==) PAny PAny = True
    (==) (PHeadTail ah at) (PHeadTail bh bt) =
        ah == bh && at == bt
    (==) (PMessage a) (PMessage b) = a == b
    (==) (PList aps) (PList bps) =
        length aps == length bps && and (zipWith (==) aps bps)
    (==) (PMatch a) (PMatch b) = a == b
    (==) (PNamed _ a) (PNamed _ b) = a == b
    (==) (PNamed _ a) b = a == b
    (==) a (PNamed _ b) = a == b
    (==) (PPMKeyword ans aps) (PPMKeyword bns bps) =
        ans == bns && and (zipWith (==) aps bps)
    (==) PThis PThis = True
    (==) _ _ = False


instance Eq Expr where
    (==) (Define _ ap' ae) (Define _ bp be) = ap' == bp && ae == be
    (==) (Set _ ap' ae) (Set _ bp be) = ap' == bp && ae == be
    (==) (Dispatch _ am) (Dispatch _ bm) = am == bm
    (==) (Operator _ ans aa ap') (Operator _ bns ba bp) =
        ans == bns && aa == ba && ap' == bp
    (==) (Primitive _ a) (Primitive _ b) = a == b
    (==) (EBlock _ aas aes) (EBlock _ bas bes) =
        aas == bas && aes == bes
    (==) (EList _ aes) (EList _ bes) = aes == bes
    (==) (EParticle _ ap') (EParticle _ bp) = ap' == bp
    (==) (ETop _) (ETop _) = True
    (==) (EVM {}) (EVM {}) = False
    (==) _ _ = False


instance Show Channel where
    show _ = "Channel"

instance Show ORef where
    show _ = "ORef"

instance Show Continuation where
    show _ = "Continuation"

instance Show (VM a) where
    show _ = "VM"

instance Typeable (VM a) where
    typeOf _ = mkTyConApp (mkTyCon "VM") [typeOf ()]


instance S.Lift Expr where
    lift (Define _ p e) = [| Define Nothing p e |]
    lift (Set _ p e) = [| Set Nothing p e |]
    lift (Dispatch _ m) = [| Dispatch Nothing m |]
    lift (Operator _ ns a p) = [| Operator Nothing ns a p |]
    lift (Primitive _ v) = [| Primitive Nothing v |]
    lift (EBlock _ as es) = [| EBlock Nothing as es |]
    lift (EVM {}) = error "cannot lift EVM"
    lift (EList _ es) = [| EList Nothing es |]
    lift (ETop _) = [| ETop Nothing |]
    lift (EParticle _ p) = [| EParticle Nothing p |]
    lift (EMacro _ p e) = [| EMacro Nothing p e |]
    lift (EForMacro _ e) = [| EForMacro Nothing e |]
    lift (EQuote _ e) = [| EQuote Nothing e |]
    lift (EUnquote _ e) = [| EUnquote Nothing e |]

instance S.Lift Assoc where
    lift ALeft = [| ALeft |]
    lift ARight = [| ARight |]

instance (S.Lift v) => S.Lift (Message v) where
    lift (Keyword i ns vs) = [| Keyword i ns vs |]
    lift (Single i n v) = [| Single i n v |]

instance (S.Lift v) => S.Lift (Particle v) where
    lift (PMSingle n) = [| PMSingle n |]
    lift (PMKeyword ns vs) = [| PMKeyword ns vs |]

instance S.Lift Value where
    lift (Block (s, as, es)) = [| Block (s, as, es) |]
    lift (Boolean b) = [| Boolean b |]
    lift (Char c) = [| Char c |]
    lift (Double d) = [| Double $(return $ S.LitE (S.RationalL (toRational d))) |]
    lift (Expression e) = [| Expression e |]
    lift (Integer i) = [| Integer i |]
    lift (Message m) = [| Message m |]
    lift (Particle p) = [| Particle p |]
    lift (Pattern p) = [| Pattern p |]
    lift (String s) = [| String (T.pack $(return $ S.LitE (S.StringL (T.unpack s)))) |]
    lift v = error $ "no lift for: " ++ show v

instance S.Lift Pattern where
    lift PAny = [| PAny |]
    lift (PHeadTail h t) = [| PHeadTail h t |]
    lift (PMessage m) = [| PMessage m |]
    lift (PList ps) = [| PList ps |]
    lift (PMatch v) = [| PMatch v |]
    lift (PNamed n p) = [| PNamed n p |]
    lift (PObject e) = [| PObject e |]
    lift (PPMKeyword ns ts) = [| PPMKeyword ns ts |]
    lift PThis = [| PThis |]
    lift PEDispatch = [| PEDispatch |]
    lift PEOperator = [| PEOperator |]
    lift PEPrimitive = [| PEPrimitive |]
    lift PEBlock = [| PEBlock |]
    lift PEList = [| PEList |]
    lift PEMacro = [| PEMacro |]
    lift PEParticle = [| PEParticle |]
    lift PETop = [| PETop |]
    lift PEQuote = [| PEQuote |]
    lift PEUnquote = [| PEUnquote |]
    lift (PExpr e) = [| PExpr e |]
    lift (PInstance p) = [| PInstance p |]
    lift (PStrict p) = [| PStrict p |]


-- | Initial "empty" parser state.
startParserState :: ParserState
startParserState = ParserState [] (M.empty, M.empty) False 0 (error "no parser environment")

-- | Initial "empty" environment state.
startEnv :: Env
startEnv = Env
    { top = error "top object not set"
    , primitives =
        IDs
            { idObject = error "idObject not set"
            , idBlock = error "idBlock not set"
            , idBoolean = error "idBoolean not set"
            , idChar = error "idChar not set"
            , idContinuation = error "idContinuation not set"
            , idDouble = error "idDouble not set"
            , idExpression = error "idExpression not set"
            , idHaskell = error "idHaskell not set"
            , idInteger = error "idInteger not set"
            , idList = error "idList not set"
            , idMessage = error "idMessage not set"
            , idMethod = error "idMethod not set"
            , idParticle = error "idParticle not set"
            , idProcess = error "idProcess not set"
            , idPattern = error "idPattern not set"
            , idRational = error "idRational not set"
            , idString = error "idString not set"
            }
    , channel = error "channel not set"
    , halt = error "halt not set"
    , loadPath = []
    , loaded = []
    , stack = []
    , parserState = startParserState
    }

-- | Evaluate x with e as the environment.
runWith :: VM Value -> Env -> IO Value
runWith x = evalStateT (runContT x return)

-- | Evaluate x with e as the environment.
runVM :: VM Value -> Env -> IO (Value, Env)
runVM x = runStateT (runContT x return)


-----------------------------------------------------------------------------
-- Helpers ------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Create a single particle with a given name.
particle :: String -> Value
{-# INLINE particle #-}
particle = Particle . PMSingle

-- | Create a keyword particle with a given name and optional values.
keyParticle :: [String] -> [Maybe Value] -> Value
{-# INLINE keyParticle #-}
keyParticle ns vs = Particle $ PMKeyword ns vs

-- | Create a keyword particle with no first role and the given values.
keyParticleN :: [String] -> [Value] -> Value
{-# INLINE keyParticleN #-}
keyParticleN ns vs = keyParticle ns (Nothing:map Just vs)

-- | Convert a String into a Value.
string :: String -> Value
{-# INLINE string #-}
string = String . T.pack

-- | Convert a Typeable value into a Haskell Value.
haskell :: Typeable a => a -> Value
{-# INLINE haskell #-}
haskell = Haskell . toDyn

-- | Convert a list of values into a List Value.
list :: [Value] -> Value
{-# INLINE list #-}
list = List . V.fromList

-- | Convert a Text string into a String.
fromText :: T.Text -> String
{-# INLINE fromText #-}
fromText = T.unpack

-- | Convert a List Value into a list of Values.
fromList :: Value -> [Value]
fromList (List vr) = V.toList vr
fromList v = error $ "no fromList for: " ++ show v

-- | Create a single message with a given name and target.
single :: String -> v -> Message v
{-# INLINE single #-}
single n = Single (hash n) n

-- | Create a keyword message with a given name and targets.
keyword :: [String] -> [v] -> Message v
{-# INLINE keyword #-}
keyword ns = Keyword (hash ns) ns

-- | Create a single message pattern with a given name and target pattern.
psingle :: String -> Pattern -> Pattern
{-# INLINE psingle #-}
psingle n = PMessage . single n

-- | Create a keyword message pattern with a given name and target patterns.
pkeyword :: [String] -> [Pattern] -> Pattern
{-# INLINE pkeyword #-}
pkeyword ns = PMessage . keyword ns

-- | Convert an AtomoError into the Value we want to error with.
asValue :: AtomoError -> Value
asValue (Error v) = v
asValue (ParseError pe) =
    keyParticleN ["parse-error"] [string (show pe)]
asValue (DidNotUnderstand m) =
    keyParticleN ["did-not-understand"] [Message m]
asValue (Mismatch pat v) =
    keyParticleN
        ["pattern", "did-not-match"]
        [Pattern pat, v]
asValue (ImportError (H.UnknownError s)) =
    keyParticleN ["unknown-hint-error"] [string s]
asValue (ImportError (H.WontCompile ges)) =
    keyParticleN ["wont-compile"] [list (nub $ map (string . H.errMsg) ges)]
asValue (ImportError (H.NotAllowed s)) =
    keyParticleN ["not-allowed"] [string s]
asValue (ImportError (H.GhcException s)) =
    keyParticleN ["ghc-exception"] [string s]
asValue (FileNotFound fn) =
    keyParticleN ["file-not-found"] [string fn]
asValue (ParticleArity e' g) =
    keyParticleN
        ["particle-needed", "given"]
        [Integer (fromIntegral e'), Integer (fromIntegral g)]
asValue (BlockArity e' g) =
    keyParticleN
        ["block-expected", "given"]
        [Integer (fromIntegral e'), Integer (fromIntegral g)]
asValue NoExpressions = particle "no-expressions"
asValue (ValueNotFound d v) =
    keyParticleN ["could-not-find", "in"] [string d, v]
asValue (DynamicNeeded t) =
    keyParticleN ["dynamic-needed"] [string t]

-- | Given a record of primitive IDs, get the object reference backing a value.
orefFrom :: IDs -> Value -> ORef
{-# INLINE orefFrom #-}
orefFrom _ (Reference r) = r
orefFrom ids (Block (_, _, _)) = idBlock ids
orefFrom ids (Boolean _) = idBoolean ids
orefFrom ids (Char _) = idChar ids
orefFrom ids (Continuation _) = idContinuation ids
orefFrom ids (Double _) = idDouble ids
orefFrom ids (Expression _) = idExpression ids
orefFrom ids (Haskell _) = idHaskell ids
orefFrom ids (Integer _) = idInteger ids
orefFrom ids (List _) = idList ids
orefFrom ids (Message _) = idMessage ids
orefFrom ids (Method _) = idMethod ids
orefFrom ids (Particle _) = idParticle ids
orefFrom ids (Process (_, _)) = idProcess ids
orefFrom ids (Pattern _) = idPattern ids
orefFrom ids (Rational _) = idRational ids
orefFrom ids (String _) = idString ids
