import System.Environment (getArgs)

countIncreases :: (Ord a) => [a] -> Int
countIncreases xs = sum $ zipWith cmp xs (tail xs)
    where cmp = \x y -> fromEnum (x < y)

buildSlidingWindow :: (Num a) => [a] -> [a]
buildSlidingWindow xs = zipWith3 add3 xs (tail xs) (tail $ tail xs)
    where add3 = \x y z -> x + y + z

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    let numStrings = lines content
    let nums = fmap read numStrings :: [Int]
    
    let answer1 = countIncreases nums
    let answer2 = (countIncreases . buildSlidingWindow) nums
    putStr "Number of increases: "
    print answer1
    putStr "Number of increases (Sliding Window): "
    print answer2
