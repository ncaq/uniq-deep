import qualified Data.Set     as S
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = T.getContents >>= mapM_ T.putStrLn . uniqDeep S.empty . T.lines

uniqDeep :: S.Set T.Text -> [T.Text] -> [T.Text]
uniqDeep _ [] = []
uniqDeep table (l : ls) = if S.member l table
                          then uniqDeep table ls
                          else l : uniqDeep (S.insert l table) ls
