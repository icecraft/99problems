

import Control.Monad 
import Data.List


-- Problem 46 
-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; 
{-
    (table A B (and A (or A B)))
    true true true
    true fail true
    fail true fail
    fail fail fail
-}

_and :: Bool -> Bool -> Bool 
_and True True = True 
_and _  _ = False 


_or :: Bool -> Bool -> Bool 
_or False False = False 
_or _ _ = True 


_nand :: Bool -> Bool -> Bool 
_nand a b = not $ _and a b 


_nor :: Bool -> Bool -> Bool 
_nor a b  = not $ _or a b 


_xor :: Bool -> Bool -> Bool 
_xor True True = False 
_xor False False = False 
_xor _ _ = True 


_equ :: Bool -> Bool -> Bool
_equ True True = True 
_equ False False = True 
_equ _ _ = False 


impl :: Bool -> Bool -> Bool
impl a b = _or (not a) b


table :: (Bool -> Bool -> Bool) -> IO()
table f = putStrLn $ concatMap (++ "\n") 
            [
                show a ++ " " ++ show b ++ " " ++ show (f a b) 
                |
                a <-[ True, False], b <- [True, False]
             ]


-- Problem 47 
-- Truth tables for logical expressions (2). 
{-

 (table A B (A and (A or not B)))
    true true true
    true fail true
    fail true fail
    fail fail fail
-}

infixr 2 `_or` 
infixr 3 `_and`



-- Problem 48 
-- Truth tables for logical expressions (3). 
{-
    (table (A,B,C) (A and (B or C) equ A and B or A and C))
        true true true true
        true true fail true
        true fail true true
        true fail fail true
        fail true true true
        fail true fail true
        fail fail true true
        fail fail fail true
-}

tableN :: Int -> ([Bool]-> Bool) -> IO()
tableN n f = putStrLn $ concatMap (++ "\n") [ render $ arg ++ [f arg ] | arg <- args n ]
        where 
            args c = replicateM c [True, False]
            render :: [Bool] -> String 
            render = intercalate " " . map show 




-- Problem 49 
-- Gray codes. 
{-
    n = 1: C(1) = ['0','1'].
    n = 2: C(2) = ['00','01','11','10'].
    n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
-}

gray :: Int -> [String] 
gray 1 = ["0", "1"]
gray n = ts ++ ds 
        where 
            pre = gray (n-1)
            ts = map (++ "0") pre
            ds = map (++ "1") $ reverse pre 
        


-- Problem 50
-- Huffman codes. 
{-
     huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
     =
	[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]    
-}


data HuffmanTree =  HTS Char Int | HTL Char | HTB Int (HuffmanTree, HuffmanTree) deriving (Show)

getHuffmanFreq :: HuffmanTree -> Int 
getHuffmanFreq (HTS _ c) = c
getHuffmanFreq (HTL _) = 0
getHuffmanFreq (HTB c _) = c

getHuffmanC :: HuffmanTree -> Char 
getHuffmanC (HTS c _) = c 
getHuffmanC (HTL c) = c 
getHuffmanC _ = '@'


extractHuffmanTree :: HuffmanTree -> [HuffmanTree]
extractHuffmanTree (HTB _ (a ,b)) = [a, b]
extractHuffmanTree _ =[]

convertToHuffmanT :: [(Char, Int)] -> [HuffmanTree]
convertToHuffmanT = map  conv 
        where conv (ch, c) = HTS ch c 


buildHuffmanTree :: [HuffmanTree] -> HuffmanTree 
buildHuffmanTree xs
                | 2 > length xs = head xs
                | otherwise = let sxs = sortOn getHuffmanFreq xs 
                                  fstE = head sxs 
                                  sndE = head $ tail sxs 
                                  rest = tail $ tail sxs
                                  freq = (getHuffmanFreq fstE) + (getHuffmanFreq sndE)
                              in buildHuffmanTree (HTB freq (fstE, sndE):rest)
      
encodeHuffmanTree :: HuffmanTree -> [(Char, String)]
encodeHuffmanTree = concatMap (render "")  
        where 
            render prefix 


-- (getHuffmanC le, prefix ++ "0"):(encode (prefix ++ "1")  (extractHuffmanTree $ head ys ))




















