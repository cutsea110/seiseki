import Data.Bool (bool)
import Data.Char (isDigit)

data Grade = A | B | C | F deriving Show
data ABCF = ABCF { numA :: Int, numB :: Int, numC :: Int, numF :: Int } deriving Show

initABCF :: ABCF
initABCF = ABCF 0 0 0 0

readInt :: String -> Maybe Int
readInt s = bool Nothing (Just n) $ not (null s) && all isDigit s && n <= 100
  where
    n = read s

convTo :: Int -> Grade
convTo n | n >=  80  = A
         | n >=  70  = B
         | n >=  60  = C
         | otherwise = F

strToGrade :: String -> Maybe Grade
strToGrade = fmap convTo . readInt

getGrade :: IO (Maybe Grade)
getGrade = fmap strToGrade getLine

putPrompt :: IO ()
putPrompt = putStr "Put a grade here: "

infixl 1 >>>
(foo >>> bar) g = foo g >> bar g

loop :: ABCF -> IO ()
loop res = putPrompt >> getGrade >>= maybe (print res) (print >>> loop . accum res)

accum :: ABCF -> Grade -> ABCF
accum res@(ABCF a b c f) g
  = case g of
      A -> res { numA = a+1 }
      B -> res { numB = b+1 }
      C -> res { numC = c+1 }
      F -> res { numF = f+1 }

main :: IO ()
main = loop initABCF
