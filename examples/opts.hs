import Data.List (sort)
import SimpleCmdArgs
import System.Directory

main =
  simpleCmdArgs Nothing "An example cli tool" "Description..." $
  ls <$> switchWith 'a' "all" "include hidden files" <*> strArg "FILE..."

ls :: Bool -> FilePath -> IO ()
ls hidden fp = sort <$>
  (if hidden then getDirectoryContents else listDirectory) fp >>= mapM_ putStrLn
