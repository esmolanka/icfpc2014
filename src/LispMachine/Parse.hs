{-# LANGUAGE RecordWildCards #-}
module LispMachine.Parse where

import Control.Applicative hiding ((<|>))
import Control.Arrow
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

import LispMachine.Instructions

lispAsmDef :: LanguageDef st
lispAsmDef = emptyDef
   { P.commentStart = "/*"
   , P.commentEnd   = "*/"
   , P.commentLine  = ";"
   , P.nestedComments = False
   , P.identStart  = alphaNum <|> oneOf "-_<>%$&*@!#"
   , P.identLetter = alphaNum <|> oneOf "-_<>%$&*@!#"
   , P.opStart     = oneOf ":"
   , P.opLetter    = alphaNum <|> oneOf "-_<>%$&*@!#:"
   , P.reservedNames = ["LDC","LD","ADD","SUB","MUL","DIV","CEQ","CGT","CGTE","ATOM"
                     ,"CONS","CAR","CDR","SEL","JOIN","LDF","AP","RTN","DUM","RAP"
                     ,"STOP","TSEL","TAP","TRAP","DBUG","BRK"]
   , P.reservedOpNames = [":"]
   , P.caseSensitive = False
   }

lispAsm :: P.TokenParser st
lispAsm = P.makeTokenParser lispAsmDef

whiteSpace :: Parsec String u ()
whiteSpace = P.whiteSpace lispAsm

reserved :: String -> Parsec String u ()
reserved = P.reserved lispAsm

reservedOp :: String -> Parsec String u ()
reservedOp = P.reservedOp lispAsm

identifier :: Parsec String u String
identifier = P.identifier lispAsm

int :: Parsec String u Int
int = fromIntegral <$> P.integer lispAsm

ref :: Parsec String u Ref
ref = choice [ At <$> (Addr <$> int)
             , Ref <$> identifier
             ]

statement :: Parsec String u Statement
statement = whiteSpace *> choice
  [ SetLabel <$> identifier <* reservedOp ":"
  , Instr <$> instruction
  ] <* whiteSpace

instruction :: Parsec String u (Instruction Ref)
instruction = choice
  [ LDC <$ reserved "LDC" <*> int
  , LD <$ reserved "LD" <*> int <*> int
  , ADD <$ reserved "ADD"
  , SUB <$ reserved "SUB"
  , MUL <$ reserved "MUL"
  , DIV <$ reserved "DIV"
  , CEQ <$ reserved "CEQ"
  , CGT <$ reserved "CGT"
  , CGTE <$ reserved "CGTE"
  , ATOM <$ reserved "ATOM"
  , CONS <$ reserved "CONS"
  , CAR <$ reserved "CAR"
  , CDR <$ reserved "CDR"
  , SEL <$ reserved "SEL" <*> ref <*> ref
  , JOIN <$ reserved "JOIN"
  , LDF <$ reserved "LDF" <*> ref
  , AP <$ reserved "AP" <*> int
  , RTN <$ reserved "RTN"
  , DUM <$ reserved "DUM" <*> int
  , RAP <$ reserved "RAP" <*> int
  , STOP <$ reserved "STOP"
  , TSEL <$ reserved "TSEL" <*> ref <*> ref
  , TAP <$ reserved "TAP" <*> int
  , TRAP <$ reserved "TRAP" <*> int
  , DBUG <$ reserved "DBUG"
  , BRK <$ reserved "BRK"
  ]

lispAsmParser :: Parsec String u Program
lispAsmParser = Program <$> sepEndBy1 statement whiteSpace

parseLispAsm :: String -> String -> Either String Program
parseLispAsm fn src = left show $ parse lispAsmParser fn src

_test :: String
_test = unlines
  [ " DUM  2        ; 2 top-level declarations"
  , "  LDF  go       ; declare function go"
  , "  LDF  to       ; declare function to"
  , "  LDF  main     ; main function"
  , "  RAP  2        ; load declarations into environment and run main"
  , "  RTN           ; final return"
  , "main:"
  , "  LDC  1"
  , "  LD   0 0      ; var go"
  , "  AP   1        ; call go(1)"
  , "  RTN"
  , "to:"
  , "  LD   0 0      ; var n"
  , "  LDC  1"
  , "  SUB"
  , "  LD   1 0      ; var go"
  , "  AP   1        ; call go(n-1)"
  , "  RTN"
  , "go:"
  , "  LD   0 0      ; var n"
  , "  LDC  1"
  , "  ADD"
  , "  LD   1 1      ; var to"
  , "  AP   1        ; call to(n+1)"
  , "  RTN"
  ]
