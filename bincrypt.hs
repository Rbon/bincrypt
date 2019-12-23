import Data.Char (ord, intToDigit)
import System.Environment

type Bin = [Int]

main = getArgs >>= parseArgs >>= putStrLn

parseArgs :: [String] -> IO String
parseArgs (x:xs) = return $ bincrypt x
parseArgs []     = getContents

bincrypt :: String -> String
bincrypt = map intToDigit . strToBin

strToBin :: String -> Bin
strToBin = concatMap charToBin

charToBin :: Char -> Bin
charToBin = intToBin . ord

-- I have no idea how this works
intToBin :: Int -> Bin
intToBin 0 = [0]
intToBin n = intToBin (n `quot` 2) ++ [n `rem` 2]
