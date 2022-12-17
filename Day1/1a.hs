import System.Environment
import Data.List
import Data.List.Split (splitOn)

main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let contentblocks = blocks contents
    putStrLn $ "The maximum calories carried by an elf is: " ++ show (maxOfList $ blockSums contentblocks) 

blocks :: String -> [[String]]
blocks x = map lines (splitOn "\n\n" x)

blockSums :: [[String]] -> [Int]
blockSums b = map (sum . readIntList) b

maxOfList :: [Int] -> Int
maxOfList ints = foldr1 (\x y -> if x >= y then x else y) ints

readIntList :: [String] -> [Int]
readIntList = map (\x -> read x :: Int)
