{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Atomo.Parser.Base where

import Control.Monad.Identity
import Data.Char
import Data.Hashable (hash)
import Data.List (nub, sort)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

import Atomo.Types (Expr(..))


opLetters :: [Char]
opLetters = "~!@#$%^&*-_=+./\\|<>?"

def :: P.GenLanguageDef String a Identity
def = P.LanguageDef
    { P.commentStart = "{-"
    , P.commentEnd = "-}"
    , P.commentLine = "--"
    , P.nestedComments = True
    , P.identStart = letter <|> oneOf "_"
    , P.identLetter = alphaNum <|> P.opLetter def
    , P.opStart = oneOf opLetters
    , P.opLetter = alphaNum <|> oneOf opLetters
    , P.reservedOpNames = ["=", ":=", ",", "|", "_"]
    , P.reservedNames = ["dispatch"]
    , P.caseSensitive = True
    }

tp :: P.GenTokenParser String () Identity
tp = makeTokenParser def

lexeme :: Parser a -> Parser a
lexeme = P.lexeme tp

capIdent :: Parser String
capIdent = do
    c <- satisfy isUpper
    cs <- many (P.identLetter def)
    return (c:cs)

lowIdent :: Parser String
lowIdent = do
    c <- satisfy isLower
    cs <- many (P.identLetter def)
    return (c:cs)

capIdentifier :: Parser String
capIdentifier = lexeme capIdent

lowIdentifier :: Parser String
lowIdentifier = lexeme lowIdent

anyIdent :: Parser String
anyIdent = do
    c <- P.identStart def
    cs <- many (P.identLetter def)
    return (c:cs)

anyIdentifier :: Parser String
anyIdentifier = lexeme anyIdent

parens :: Parser a -> Parser a
parens = P.parens tp

brackets :: Parser a -> Parser a
brackets = P.brackets tp

braces :: Parser a -> Parser a
braces = P.braces tp

comma :: Parser String
comma = P.comma tp

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep tp

commaSep1 :: Parser a -> Parser [a]
commaSep1 = P.commaSep1 tp

dot :: Parser String
dot = P.dot tp

identifier :: Parser String
identifier = lexeme $ P.identifier tp

ident :: Parser String
ident = P.identifier tp

operator :: Parser String
operator = do
    c <- P.opStart def
    cs <- many (P.opLetter def)
    if (c:cs) `elem` P.reservedOpNames def
        then unexpected ("reserved operator " ++ show (c:cs))
        else return (c:cs)

reserved :: String -> Parser ()
reserved = P.reserved tp

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp tp

integer :: Parser Integer
integer = do
    f <- sign
    n <- natural
    return (f n)
  where
    sign = choice
        [ char '-' >> return negate
        , char '+' >> return id
        , return id
        ]

float :: Parser Double
float = do
    f <- sign
    n <- P.float tp
    return (f n)
  where
    sign = choice
        [ char '-' >> return negate
        , char '+' >> return id
        , return id
        ]

natural :: Parser Integer
natural = P.natural tp

symbol :: String -> Parser String
symbol = P.symbol tp

delimit :: String -> Parser String
delimit n = whiteSpace >> symbol n

stringLiteral :: Parser String
stringLiteral = P.stringLiteral tp

charLiteral :: Parser Char
charLiteral = P.charLiteral tp

colon :: Parser ()
colon = char ':' >> return ()

wsBlock :: Show a => Parser a -> Parser [a]
wsBlock = indentAware (\n o -> sourceColumn n == sourceColumn o) (delimit ";" >> return True) False

wsMany1 :: Show a => Parser a -> Parser [a]
wsMany1 p = do
    ps <- indentAware chainContinue (return False) True p
    if null ps
        then fail "needed more than one"
        else return ps

wsMany :: Show a => Parser a -> Parser [a]
wsMany = indentAware chainContinue (return False) True

wsManyStart :: Show a => Parser a -> Parser a -> Parser [a]
wsManyStart s p = do
    ps <- indentAwareStart chainContinue (return False) True s p
    if null ps
        then fail "needed more than one"
        else return ps

chainContinue :: SourcePos -> SourcePos -> Bool
chainContinue n o = sourceLine o == sourceLine n || sourceColumn n > sourceColumn o

indentAware :: Show a => (SourcePos -> SourcePos -> Bool) -> Parser Bool -> Bool -> Parser a -> Parser [a]
indentAware cmp delim allowSeq p = indentAwareStart cmp delim allowSeq p p

indentAwareStart :: Show a => (SourcePos -> SourcePos -> Bool) -> Parser Bool -> Bool -> Parser a -> Parser a -> Parser [a]
indentAwareStart cmp delim allowSeq s p = do
    start <- getPosition
    wsmany start []
  where
    wsmany o es = choice
        [ try $ do
            x <- if null es then s else p

            new <- lookAhead (whiteSpace >> getPosition)
            sequential <- fmap (== new) $ lookAhead (spacing >> getPosition)

            delimited <- option False $ try delim

            if delimited || cmp new o || (allowSeq && sequential)
                then whiteSpace >> wsmany o (es ++ [x])
                else return (es ++ [x])
        , return es
        ]

keyword :: Parser a -> Parser (String, a)
keyword p = try $ do
    name <- try (do
        name <- ident
        char ':'
        return name) <|> operator
    whiteSpace1 -- TODO: this was space, but seeing if this works too
    target <- p
    return (name, target)

keywords :: Show a => (Int -> [String] -> [a] -> b) -> a -> Parser a -> Parser b
keywords c d p = do
    (first, ks) <- choice
        [ try $ do
            f <- p
            fs <- wsMany1 (keyword p)
            return (f, fs)
        , do
            fs <- wsMany1 (keyword p)
            return (d, fs)
        ]

    let (ns, ps) = unzip ks

    return $ c (hash ns) ns (first:ps)

tagged :: Parser Expr -> Parser Expr
tagged p = do
    pos <- getPosition
    r <- p
    return r { eLocation = Just pos }

makeTokenParser :: P.GenLanguageDef String () Identity -> P.GenTokenParser String () Identity
makeTokenParser languageDef
    = P.TokenParser{ P.identifier = identifier
                   , P.reserved = reserved
                   , P.operator = operator
                   , P.reservedOp = reservedOp

                   , P.charLiteral = charLiteral
                   , P.stringLiteral = stringLiteral
                   , P.natural = natural
                   , P.integer = integer
                   , P.float = float
                   , P.naturalOrFloat = naturalOrFloat
                   , P.decimal = decimal
                   , P.hexadecimal = hexadecimal
                   , P.octal = octal

                   , P.symbol = symbol
                   , P.lexeme = lexeme
                   , P.whiteSpace = whiteSpace

                   , P.parens = parens
                   , P.braces = braces
                   , P.angles = angles
                   , P.brackets = brackets
                   , P.squares = brackets
                   , P.semi = semi
                   , P.comma = comma
                   , P.colon = colon
                   , P.dot = dot
                   , P.semiSep = semiSep
                   , P.semiSep1 = semiSep1
                   , P.commaSep = commaSep
                   , P.commaSep1 = commaSep1
                   }
    where

    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens p        = between (open "(") (close ")") p
    braces p        = between (open "{") (close "}") p
    angles p        = between (open "<") (close ">") p
    brackets p      = between (open "[") (close "]") p

    semi            = delimit ";"
    comma           = delimit ","
    dot             = delimit "."
    colon           = delimit ":"

    commaSep p      = sepBy p comma
    semiSep p       = sepBy p semi

    commaSep1 p     = sepBy1 p comma
    semiSep1 p      = sepBy1 p semi


    -----------------------------------------------------------
    -- Chars & Strings
    -----------------------------------------------------------
    charLiteral     = lexeme (between (char '\'')
                                      (char '\'' <?> "end of character")
                                      characterChar )
                    <?> "character"

    characterChar   = charLetter <|> charEscape
                    <?> "literal character"

    charEscape      = do{ char '\\'; escapeCode }
    charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))



    stringLiteral   = lexeme (
                      do{ str <- between (char '"')
                                         (char '"' <?> "end of string")
                                         (many stringChar)
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "literal string")

    stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape
                    <?> "string character"

    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape    = do{ char '\\'
                        ;     do{ escapeGap  ; return Nothing }
                          <|> do{ escapeEmpty; return Nothing }
                          <|> do{ esc <- escapeCode; return (Just esc) }
                        }

    escapeEmpty     = char '&'
    escapeGap       = do{ many1 space
                        ; char '\\' <?> "end of string gap"
                        }



    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    charControl     = do{ char '^'
                        ; code <- upper
                        ; return (toEnum (fromEnum code - fromEnum 'A'))
                        }

    charNum         = do{ code <- decimal
                                  <|> do{ char 'o'; number 8 octDigit }
                                  <|> do{ char 'x'; number 16 hexDigit }
                        ; return (toEnum (fromInteger code))
                        }

    charEsc         = choice (map parseEsc escMap)
                    where
                      parseEsc (c,code)     = do{ char c; return code }

    charAscii       = choice (map parseAscii asciiMap)
                    where
                      parseAscii (asc,code) = try (do{ string asc; return code })


    -- escape code tables
    escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    naturalOrFloat  = lexeme (natFloat) <?> "number"

    float           = lexeme floating   <?> "float"
    integer         = lexeme int        <?> "integer"
    natural         = lexeme nat        <?> "natural"


    -- floats
    floating        = do{ n <- decimal
                        ; fractExponent n
                        }


    natFloat        = do{ char '0'
                        ; zeroNumFloat
                        }
                      <|> decimalFloat

    zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                         ; return (Left n)
                         }
                    <|> decimalFloat
                    <|> fractFloat 0
                    <|> return (Left 0)

    decimalFloat    = do{ n <- decimal
                        ; option (Left n)
                                 (fractFloat n)
                        }

    fractFloat n    = do{ f <- fractExponent n
                        ; return (Right f)
                        }

    fractExponent n = do{ fract <- fraction
                        ; expo  <- option 1.0 exponent'
                        ; return ((fromInteger n + fract)*expo)
                        }
                    <|>
                      do{ expo <- exponent'
                        ; return ((fromInteger n)*expo)
                        }

    fraction        = do{ char '.'
                        ; digits <- many1 digit <?> "fraction"
                        ; return (foldr op 0.0 digits)
                        }
                      <?> "fraction"
                    where
                      op d f    = (f + fromIntegral (digitToInt d))/10.0

    exponent'       = do{ oneOf "eE"
                        ; f <- sign
                        ; e <- decimal <?> "exponent"
                        ; return (power (f e))
                        }
                      <?> "exponent"
                    where
                       power e  | e < 0      = 1.0/power(-e)
                                | otherwise  = fromInteger (10^e)


    -- integers and naturals
    int             = do{ f <- sign
                        ; n <- nat
                        ; return (f n)
                        }

    sign            =   (char '-' >> return negate)
                    <|> (char '+' >> return id)
                    <|> return id

    nat             = zeroNumber <|> decimal

    zeroNumber      = do{ char '0'
                        ; hexadecimal <|> octal <|> decimal <|> return 0
                        }
                      <?> ""

    decimal         = number 10 digit
    hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
    octal           = do{ oneOf "oO"; number 8 octDigit  }

    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

    -----------------------------------------------------------
    -- Operators & reserved ops
    -----------------------------------------------------------
    reservedOp name =
        lexeme $ try $
        do{ string name
          ; notFollowedBy (P.opLetter languageDef) <?> ("end of " ++ show name)
          }

    operator =
        lexeme $ try $
        do{ name <- oper
          ; if (isReservedOp name)
             then unexpected ("reserved operator " ++ show name)
             else return name
          }

    oper =
        do{ c <- (P.opStart languageDef)
          ; cs <- many (P.opLetter languageDef)
          ; return (c:cs)
          }
        <?> "operator"

    isReservedOp name =
        isReserved (sort (P.reservedOpNames languageDef)) name


    -----------------------------------------------------------
    -- Identifiers & Reserved words
    -----------------------------------------------------------
    reserved name =
        lexeme $ try $
        do{ caseString name
          ; notFollowedBy (P.identLetter languageDef) <?> ("end of " ++ show name)
          }

    caseString name
        | P.caseSensitive languageDef  = string name
        | otherwise               = do{ walk name; return name }
        where
          walk []     = return ()
          walk (c:cs) = do{ caseChar c <?> msg; walk cs }

          caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                      | otherwise  = char c

          msg         = show name


    identifier =
        try $
        do{ name <- ident
          ; if (isReservedName name)
             then unexpected ("reserved word " ++ show name)
             else return name
          }


    ident
        = do{ c <- P.identStart languageDef
            ; cs <- many (P.identLetter languageDef)
            ; return (c:cs)
            }
        <?> "identifier"

    isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | P.caseSensitive languageDef  = name
                        | otherwise               = map toLower name


    isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

    theReservedNames
        | P.caseSensitive languageDef  = sortedNames
        | otherwise               = map (map toLower) sortedNames
        where
          sortedNames   = sort (P.reservedNames languageDef)



    -----------------------------------------------------------
    -- White space & symbols
    -----------------------------------------------------------
    delimit name
        = do{ whiteSpace; symbol name }

    open = symbol

    close name
        = do{ whiteSpace; s <- string name; spacing; return s }

    symbol name
        = do{ s <- string name; whiteSpace; return s }

    lexeme p
        = do{ x <- p; spacing; return x  }

    --whiteSpace
    whiteSpace = do spacing
                    skipMany (try $ spacing >> newline)
                    spacing

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace tp

whiteSpace1 :: Parser ()
whiteSpace1 = (space <|> newline) >> whiteSpace

simpleSpace :: Parser ()
simpleSpace = skipMany1 $ satisfy (`elem` " \t\f\v\xa0")

spacing :: Parser ()
spacing = skipMany spacing1

spacing1 :: Parser ()
spacing1 | noLine && noMulti  = simpleSpace <?> ""
         | noLine             = simpleSpace <|> multiLineComment <?> ""
         | noMulti            = simpleSpace <|> oneLineComment <?> ""
         | otherwise          = simpleSpace <|> oneLineComment <|> multiLineComment <?> ""
         where
             noLine  = null (P.commentLine def)
             noMulti = null (P.commentStart def)

oneLineComment :: Parser ()
oneLineComment = try (string (P.commentLine def)) >> skipMany (satisfy (/= '\n'))

multiLineComment :: Parser ()
multiLineComment = try (string (P.commentStart def)) >> inComment

inComment :: Parser ()
inComment | P.nestedComments def = inCommentMulti
          | otherwise = inCommentSingle

inCommentMulti :: Parser ()
inCommentMulti = (try (string (P.commentEnd def)) >> return ())
             <|> (multiLineComment >> inCommentMulti)
             <|> (skipMany1 (noneOf startEnd) >> inCommentMulti)
             <|> (oneOf startEnd >> inCommentMulti)
                 <?> "end of comment"
               where
                   startEnd = nub (P.commentEnd def ++ P.commentStart def)

inCommentSingle :: Parser ()
inCommentSingle = (try (string (P.commentEnd def)) >> return ())
              <|> (skipMany1 (noneOf startEnd) >> inCommentSingle)
              <|> (oneOf startEnd >> inCommentSingle)
                  <?> "end of comment"
                where
                    startEnd   = nub (P.commentEnd def ++ P.commentStart def)


