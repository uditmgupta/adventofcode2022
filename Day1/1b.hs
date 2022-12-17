import System.Environment
import Data.List
import Data.List.Split (splitOn)

main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let contentblocks = blocks contents
    putStrLn $ "The total calories of the top 3 elf food bags is " ++ show (sum $ top3ofList $ blockSums contentblocks)

blocks :: String -> [[String]]
blocks x = map lines (splitOn "\n\n" x)

blockSums :: [[String]] -> [Int]
blockSums b = map (sum . readIntList) b

maxOfList :: [Int] -> Int
maxOfList ints = foldr1 (\x y -> if x >= y then x else y) ints

readIntList :: [String] -> [Int]
readIntList = map (\x -> read x :: Int)

top3ofList :: [Int] -> [Int]
top3ofList = take 3 . reverse . sort

