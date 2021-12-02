import System.IO
import Data.List.Split (splitOn)

type Input = (String, Int)
type Horizontal = Int
type Depth = Int

dive :: [Input] -> Horizontal -> Depth -> Int
dive [] hor dep = hor * dep
dive (x:xs) hor dep = case fst x of "forward" -> dive xs (hor + snd x) dep
                                    "down"    -> dive xs hor (dep + snd x)
                                    "up"      -> dive xs hor (dep - snd x)

makeTuples :: [String] -> [Input]
makeTuples [] = []
makeTuples (x:xs) = (head splitted, read $ last splitted) : makeTuples xs
    where
        splitted = splitOn " " x

main :: IO Int
main = do
    file <- openFile "d2/input.txt" ReadMode
    input <- hGetContents file
    let lined = lines input
    let tupled = makeTuples lined
    return $ dive tupled 0 0