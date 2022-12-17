import System.Environment
import Data.List
import Data.Maybe

main = do 
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let sacks = lines contents 
    let priorities = map (getPriority . wrongItem) sacks   
    putStrLn $ "The sum of the priorities is " ++ (show $ sum priorities)

halve :: String -> (String, String)
halve s = 
    ((take half s), (drop half s))
    where 
        half = (length s) `div` 2

getPriority :: Char -> Int
getPriority c = fromMaybe 0 $ lookup c typeValues
    where typeValues = (zip ['a'..'z'] [1..26]) ++ (zip ['A'..'Z'] [27..52])   

wrongItem :: String -> Char
wrongItem s = head $ intersect comp1 comp2
    where (comp1, comp2) = halve s
