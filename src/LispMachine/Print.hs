{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module LispMachine.Print where

import Text.PrettyPrint.ANSI.Leijen

import LispMachine.Instructions

showProgram :: FlatProgram -> String
showProgram x = displayS (renderPretty 0.4 80 (ppFlatProgram x)) ""


ppFlatProgram :: FlatProgram -> Doc
ppFlatProgram (FlatProgram instrs) = vcat . map ppInstr $ instrs

instr :: Doc -> Doc
instr = fill 4

ppAddr :: AnnotatedAddr -> Doc
ppAddr (aAddr -> (Addr n)) = pretty n

ppLabel :: AnnotatedAddr -> Doc
ppLabel (aLabel -> str) = maybe "#" text str

ppComment :: Doc -> Doc
ppComment txt = toCol 15 (semi <+> txt)

toCol :: Int -> Doc -> Doc
toCol n t = column (\c -> if c < n then text (replicate (n-c) ' ') <> t else t)

ppInstr :: Instruction AnnotatedAddr -> Doc
ppInstr (LDC c)   = instr "LDC" <+> int c
ppInstr (LD n i)  = instr "LD" <+> int n <+> int i
ppInstr ADD       = instr "ADD"
ppInstr SUB       = instr "SUB"
ppInstr MUL       = instr "MUL"
ppInstr DIV       = instr "DIV"
ppInstr CEQ       = instr "CEQ"
ppInstr CGT       = instr "CGT"
ppInstr CGTE      = instr "CGTE"
ppInstr ATOM      = instr "ATOM"
ppInstr CONS      = instr "CONS"
ppInstr CAR       = instr "CAR"
ppInstr CDR       = instr "CDR"
ppInstr (SEL t f) = instr "SEL" <+> ppAddr t <+> ppAddr f <+> ppComment (ppLabel t <+> ppLabel f)
ppInstr JOIN      = instr "JOIN"
ppInstr (LDF f) = instr "LDF" <+> ppAddr f <+> ppComment (ppLabel f)
ppInstr (AP n)    = instr "AP" <+> int n
ppInstr RTN       = instr "RTN"
ppInstr (DUM n)   = instr "DUM" <+> int n
ppInstr (RAP n)   = instr "RAP" <+> int n
ppInstr STOP      = instr "STOP"
ppInstr (TSEL t f) = instr "TSEL" <+> ppAddr t <+> ppAddr f <+> ppComment (ppLabel t <+> ppLabel f)
ppInstr (TAP n)   = instr "TAP" <+> int n
ppInstr (TRAP n)  = instr "TRAP" <+> int n
ppInstr DBUG      = instr "DBUG"
ppInstr BRK       = instr "BRK"

_test :: FlatProgram
_test = FlatProgram
   [ SEL (AnnotatedAddr (Addr 1) Nothing) (AnnotatedAddr (Addr 1000) Nothing)
   , LD 10 20
   , ADD
   , LDF (AnnotatedAddr (Addr 10) (Just "foo"))
   ]

