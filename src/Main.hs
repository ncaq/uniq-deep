module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Set              as S

main :: IO ()
main = uniqDeep

uniqDeep :: IO ()
uniqDeep = B.getContents >>= B.putStrLn . B.unlines . deleteDuplicate . B.lines

deleteDuplicate :: [B.ByteString] -> [B.ByteString]
deleteDuplicate = check S.empty

check :: S.Set B.ByteString -> [B.ByteString] -> [B.ByteString]
check _ [] = []
check already (l : ls)
  | l `S.member` already = check already ls
  | otherwise            = l : check (S.insert l already) ls
