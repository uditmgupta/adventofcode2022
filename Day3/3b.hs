import System.Environment
import Data.List
import Data.Maybe
 
main = do 
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The sum of the priorities is " ++ show (sum $ map getPriority $ getBadges contents)

getBadges :: String -> [Char]
getBadges x = map (head . intersectLists') (chunksOf3 . lines $ x)  


halve :: String -> (String, String)
halve s = 
    splitAt half s
    where 
        half = (length s) `div` 2

getPriority :: Char -> Int
getPriority c = fromMaybe 0 $ lookup c typeValues
    where typeValues = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]   

wrongItem :: String -> Char
wrongItem s = head $ intersect comp1 comp2
    where (comp1, comp2) = halve s

chunksOf3 :: [a] -> [[a]]
chunksOf3 [] = []
chunksOf3 x = [take 3 x] ++ chunksOf3 (drop 3 x) 

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [[]] = []
intersectLists [[x]] = [x]
intersectLists [[x],[y]] =  [x] `intersect` [y]
intersectLists ([x]:xs) = [x] `intersect` intersectLists xs 

intersectLists' :: (Eq a) => [[a]] -> [a]
intersectLists' [] = []
intersectLists' x = foldr1 (intersect) x
