module Roman where

import Data.List.Utils (replace)
import Data.Function 

toRoman :: Int -> String
toRoman n 
  | n == 0           = ""
  | n `div` 1000 > 0 = 'M'  :  toRoman (n-1000)
  | n `div` 900 > 0  = "CM" <> toRoman (n-900)
  | n `div` 500 > 0  = 'D'  :  toRoman (n-500)
  | n `div` 400 > 0  = "CD" <> toRoman (n-400)
  | n `div` 100 > 0  = 'C'  :  toRoman (n-100)
  | n `div` 90 > 0   = "XC" <> toRoman (n-90)
  | n `div` 50 > 0   = 'L'  :  toRoman (n-50)
  | n `div` 40 > 0   = "XL" <> toRoman (n-40)
  | n `div` 10 > 0   = 'X'  :  toRoman (n-10)
  | n `div` 9 > 0    = "IX" <> toRoman (n-9)
  | n `div` 5 > 0    = 'V'  :  toRoman (n-5)
  | n `div` 4 > 0    = "IV" <> toRoman (n-4)
  | otherwise        = 'I'  :  toRoman (n-1)

{-  Pros & Cons
  - lots of repeated code, 
  - Operations performed multiple times, needlessly
  - Could capture the quotient (no ot fimtes divisible by) and use that to determine number of numerals
  - Hard to add another numeral without changing algorithm

  + Algorithm is clear (YMMV!)
-}









toRoman2 :: Int -> String 
toRoman2 n = snd $ foldr f (n, "") numerals
  where f (i, numeral) (n', str) = let (q, r) = n' `divMod` i
                                   in  (r, str <> concat (replicate q numeral))

numerals :: [(Int, String)]
numerals = [(1,"I"),(4,"IV"),(5,"V"),(9,"IX"),(10,"X"),(40,"XL"),(50,"L"),(90,"XC"),(100,"C"),(400,"CD"),(500,"D"),(900,"CM"),(1000,"M")]

{- Pros and cons:
   + no replition
   + no duplicate code
   + easy to add new numeral without changing algorithm

   - Doesn't easily reveal algorithm
-}










toRoman3 :: Int -> String
toRoman3 n = n & flip replicate 'I' & replace "IIIII" "V"  &
                                      replace "IIII"  "IV" &
                                      replace "VV"    "X"  &
                                      replace "VIV"   "IX" &
                                      replace "XXXXX" "L"  &
                                      replace "XXXX"  "XL" &
                                      replace "LL"    "C"  &
                                      replace "LXL"   "XC" &
                                      replace "CCCCC" "D"  &
                                      replace "CCCC"  "CD" &
                                      replace "DD"    "M"  &
                                      replace "DCD"   "CM"
{-  + Certainly creative!
    + Clear algorithm

    - Not very efficiennt
 -}










toRoman4 :: Int -> String
toRoman4 0 = ""
toRoman4 n = numeral ++ toRoman4 (n - numeralValue)
             where (numeralValue, numeral) = head $ filter ((<=n) . fst) $ reverse numerals

{- 
  Takes the biggest numeral greater than n, and adds it to the string, repeatedly until you get to 0!

  Another clever example
  + Short!
  - lots of temp lists created

  -}










getNum :: Char -> (Char, Char, Char) -> String
getNum n (low, mid, high) = case n of '1' -> [low]
                                      '2' -> [low, low]
                                      '3' -> [low, low, low]
                                      '4' -> [low, mid]
                                      '5' -> [mid]
                                      '6' -> [mid, low]
                                      '7' -> [mid, low, low]
                                      '8' -> [mid, low, low, low]
                                      '9' -> [low, high]
                                      _   -> []

toRoman5 :: Int -> String
toRoman5 n =  let digits = show n 
              in  mconcat $ zipWith getNum digits $ drop (4 - length digits) trips

trips :: [(Char, Char, Char)]
trips = [ ('M', ' ', ' ') -- 1000 - ?
        , ('C', 'D', 'M') -- 100 - 1000
        , ('X', 'L', 'C') -- 10 - 100
        , ('I', 'V', 'X') -- 1-10
        ]

{-
 Doesnt use any math, only working out the number of places and applies a pattern for each placeholder

 -}