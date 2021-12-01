import System.IO

increases :: [Int] -> Int
increases [] = 0
increases [x] = 0
increases (x:y:xs) = if y > x then 1 + increases (y:xs) else increases (y:xs)

main :: IO Int
main = do
    file <- openFile "d1/increasesInput.txt" ReadMode
    input <- hGetContents file
    let inc = increases (map read (lines input))
    return inc
