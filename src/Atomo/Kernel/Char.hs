{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Char where

import Data.Char

import Atomo


load :: VM ()
load = do
    [$p|(a: Char) control?|] =: lift1 isControl
    [$p|(a: Char) space?|] =: lift1 isSpace
    [$p|(a: Char) lower?|] =: lift1 isLower
    [$p|(a: Char) upper?|] =: lift1 isUpper
    [$p|(a: Char) alpha?|] =: lift1 isAlpha
    [$p|(a: Char) alphanum?|] =: lift1 isAlphaNum
    [$p|(a: Char) print?|] =: lift1 isPrint
    [$p|(a: Char) digit?|] =: lift1 isDigit
    [$p|(a: Char) oct-digit?|] =: lift1 isOctDigit
    [$p|(a: Char) hex-digit?|] =: lift1 isHexDigit
    [$p|(a: Char) letter?|] =: lift1 isLetter
    [$p|(a: Char) mark?|] =: lift1 isMark
    [$p|(a: Char) number?|] =: lift1 isNumber
    [$p|(a: Char) punctuation?|] =: lift1 isPunctuation
    [$p|(a: Char) symbol?|] =: lift1 isSymbol
    [$p|(a: Char) separator?|] =: lift1 isSeparator
    [$p|(a: Char) ascii?|] =: lift1 isAscii
    [$p|(a: Char) latin1?|] =: lift1 isLatin1
    [$p|(a: Char) ascii-upper?|] =: lift1 isAsciiLower
    [$p|(a: Char) ascii-lower?|] =: lift1 isAsciiUpper

    [$p|(a: Char) uppercase|] =: lift1 toUpper
    [$p|(a: Char) lowercase|] =: lift1 toLower
    [$p|(a: Char) titlecase|] =: lift1 toTitle

    [$p|(a: Char) from-digit|] =: lift1 (toInteger . digitToInt)
    [$p|(a: Integer) to-digit|] =: lift1 (intToDigit . fromInteger)

    [$p|(a: Char) ord|] =: lift1 (toInteger . ord)
    [$p|(a: Integer) chr|] =: lift1 (chr . fromInteger)

    [$p|(a: Char) category|] =: lift1 (c.generalCategory)
  where
    c UppercaseLetter = keyParticleN ["letter"] [particle "lowercase"]
    c LowercaseLetter = keyParticleN ["letter"] [particle "uppercase"]
    c TitlecaseLetter = keyParticleN ["letter"] [particle "titlecase"]
    c ModifierLetter = keyParticleN ["letter"] [particle "modified"]
    c OtherLetter = keyParticleN ["letter"] [particle "other"]
    c NonSpacingMark = keyParticleN ["mark"] [particle "non-spacing"]
    c SpacingCombiningMark = keyParticleN ["mark"] [particle "space-combining"]
    c EnclosingMark = keyParticleN ["mark"] [particle "enclosing"]
    c DecimalNumber = keyParticleN ["number"] [particle "decimal"]
    c LetterNumber = keyParticleN ["number"] [particle "letter"]
    c OtherNumber = keyParticleN ["number"] [particle "other"]
    c ConnectorPunctuation = keyParticleN ["punctuation"] [particle "connector"]
    c DashPunctuation = keyParticleN ["punctuation"] [particle "dash"]
    c OpenPunctuation = keyParticleN ["punctuation"] [particle "open"]
    c ClosePunctuation = keyParticleN ["punctuation"] [particle "close"]
    c InitialQuote = keyParticleN ["quote"] [particle "initial"]
    c FinalQuote = keyParticleN ["quote"] [particle "final"]
    c OtherPunctuation = keyParticleN ["punctuation"] [particle "other"]
    c MathSymbol = keyParticleN ["symbol"] [particle "math"]
    c CurrencySymbol = keyParticleN ["symbol"] [particle "currency"]
    c ModifierSymbol = keyParticleN ["symbol"] [particle "modifier"]
    c OtherSymbol = keyParticleN ["symbol"] [particle "other"]
    c Space = particle "space"
    c LineSeparator = keyParticleN ["separator"] [particle "line"]
    c ParagraphSeparator = keyParticleN ["separator"] [particle "paragraph"]
    c Control = particle "control"
    c Format = particle "format"
    c Surrogate = particle "surrogate"
    c PrivateUse = particle "private-use"
    c NotAssigned = particle "not-assigned"
