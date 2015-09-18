import qualified Data.ByteString.Char8 as B
import qualified Data.Set              as S

main :: IO ()
main = B.getContents >>= mapM_ B.putStrLn . uniqDeep S.empty . B.lines

uniqDeep :: S.Set B.ByteString -> [B.ByteString] -> [B.ByteString]
uniqDeep _ [] = []
uniqDeep table (l : ls) = if S.member l table
                          then uniqDeep table ls
                          else l : uniqDeep (S.insert l table) ls
