import System.IO

-- off by one algorightm
increases2 :: [Int] -> Int -> Int
increases2 [] prev = 0
increases2 [x,y] prev = 0
increases2 [x,y,z] prev = if x+y+z >= prev then 1 else 0
increases2 (x:y:z:xs) prev = if x + y + z > prev 
                             then 1 + increases2 (y:z:xs) (x+y+z)
                             else increases2 (y:z:xs) (x+y+z)

main :: IO Int
main = do
    file <- openFile "d1/increasesInput.txt" ReadMode
    input <- hGetContents file
    let inc = increases2 (map read (lines input)) 0
    return (inc - 1)