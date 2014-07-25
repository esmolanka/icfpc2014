module GHC.DSL where


-- Some examples

miner = do
  go Down
  halt

getMyLocation =
  getMyIndex >>= getLocationOf

flipper = do
  (x,_y) <- getMyLocation
  x := x & 1
  if’ (x == 1)
    (go Up)
    (go Down)
  halt

fickle = do
   a := inifinity
   b := 0
   c := (-1)
   while $ do
     c := c + 1 -- 0 <= c <= 3
     if1 (mem c <= a) $ do
       a := mem c
       b := c
     if1 (c < 3)
       break

   go (toDirection b)

   dir <- getMyDirection
   inc (mem dir)
   halt
