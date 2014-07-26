
module ListMachine.Parse where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

lispAsmDef :: LanguageDef st
lispAsmDef = emptyDef
   { commentStart = "/*"
   , commentEnd   = "*/"
   , commentLine  = ";"
   , nestedComments = False
   , identStart  = alphaNum <|> oneOf "-_<>%$&*@!#"
   , identLetter = alphaNum <|> oneOf "-_<>%$&*@!#"
   , opStart     = oneOf ":"
   , opLetter    = alphaNum <|> oneOf "-_<>%$&*@!#:"
   , reservedNames = ["LDC","LD","ADD","SUB","MUL","DIV","CEQ","CGT","CGTE","ATOM"
                     ,"CONS","CAR","CDR","SEL","JOIN","LDF","AP","RTN","DUM","RAP"
                     ,"STOP","TSEL","TAP","TRAP","DBUG","BRK"]
   , reservedOpNames = [":"]
   , caseSensitive = False
   }

