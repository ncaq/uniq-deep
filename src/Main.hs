import qualified Data.ByteString.Char8 as B
import qualified Data.Set              as S

main :: IO ()
main = uniqDeep

uniqDeep :: IO ()
uniqDeep = B.putStrLn . B.unlines . deleteDuplicate . B.lines =<< B.getContents

deleteDuplicate :: [B.ByteString] -> [B.ByteString]
deleteDuplicate = check S.empty
  where check _ [] = []
        check already (l : ls) | S.member l already =
                                 check already ls
                               | otherwise =
                                 l : check (S.insert l already) ls
