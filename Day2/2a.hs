import System.Environment
import Data.List

main = do 
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let total = sum $ map scoreGame (lines contents) 
    putStrLn $ "The total score is " ++ (show total)


scoreGame :: String -> Int
scoreGame g = (playedScore g) + (outcomeScore g)

playedScore :: String -> Int
playedScore g
    | x == 'X' = 1
    | x == 'Y' = 2
    | x == 'Z' = 3
    where x = last g

outcomeScore :: String -> Int
outcomeScore g = case x of
            'A' -> case y of
                'X' -> 3   
                'Y' -> 6
                'Z' -> 0 
            'B' -> case y of
                'X' -> 0   
                'Y' -> 3
                'Z' -> 6 
            'C' -> case y of
                'X' -> 6   
                'Y' -> 0
                'Z' -> 3 
            where
                x = head g
                y = last g
