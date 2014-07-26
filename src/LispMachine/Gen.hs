
module LispMachine.Gen where

import Control.Monad.RWS hiding (ap)

import LispMachine.Instructions
import LispMachine.Flatten
import LispMachine.Print

type Env = ()

data GenState = GenState
    { lastLabelId :: Int
    } deriving (Show)

newtype RRef = RRef Int deriving (Show)

toLabel :: RRef -> Label
toLabel (RRef i) = "lbl_" ++ show i

toRefLabel :: RRef -> Ref
toRefLabel = Ref . toLabel

type GenM = RWST Env [Statement] GenState (Either String)

putI :: Instruction Ref -> GenM ()
putI = tell . (:[]) . Instr

putLabel :: RRef -> GenM ()
putLabel = tell . (:[]) . SetLabel . toLabel

ldc :: Int -> GenM ()
ldc x = putI $ LDC x

ld :: Int -> Int -> GenM ()
ld n i = putI $ LD n i

st :: Int -> Int -> GenM ()
st n i = putI $ ST n i

add :: GenM ()
add = putI ADD

sub :: GenM ()
sub = putI SUB

mul :: GenM ()
mul = putI MUL

div :: GenM ()
div = putI DIV

ceq  :: GenM ()
ceq = putI CEQ

cgt  :: GenM ()
cgt = putI CGT

cgte :: GenM ()
cgte = putI CGTE

atom :: GenM ()
atom = putI ATOM

cons :: GenM ()
cons = putI CONS

car  :: GenM ()
car = putI CAR

cdr  :: GenM ()
cdr = putI CDR

sel  :: RRef -> RRef -> GenM ()
sel t f = putI $ SEL (toRefLabel t) (toRefLabel f)

join :: GenM ()
join = putI JOIN

ldf  :: RRef -> GenM ()
ldf f = putI $ LDF (toRefLabel f)

ap :: Int -> GenM ()
ap n = putI $ AP n

rtn  :: GenM ()
rtn = putI RTN

dum  :: Int -> GenM ()
dum n = putI $ DUM n

rap  :: Int -> GenM ()
rap n = putI $ RAP n

stop :: GenM ()
stop = putI STOP

tsel :: RRef -> RRef -> GenM ()
tsel t f = putI $ TSEL (toRefLabel t) (toRefLabel f)

tap  :: Int -> GenM ()
tap n = putI $ TAP n

trap :: Int -> GenM ()
trap n = putI $ TRAP n

dbug :: GenM ()
dbug = putI DBUG

brk  :: GenM ()
brk = putI BRK

mkLabel :: GenM RRef
mkLabel = do
  last <- gets lastLabelId
  modify (\s -> s {lastLabelId = succ last})
  return $ RRef $ succ last

block :: RRef -> GenM a -> GenM a
block ref gen = do
  putLabel ref
  gen

initEnv :: ()
initEnv = ()

initState :: GenState
initState = GenState { lastLabelId = 0 }

genProgram :: GenM () -> Either String Program
genProgram gen = fmap (Program . thrd) $ runRWST gen initEnv initState
    where thrd (_,_,x) = x

genToString :: GenM () -> String
genToString = showProgram . flatten . either error id . genProgram
