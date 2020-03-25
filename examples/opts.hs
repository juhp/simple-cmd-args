import Data.List (sort)
import SimpleCmdArgs
import System.Directory

main =
  simpleCmdArgs Nothing "lsdir" "Lists filepath..." $
  ls <$> switchWith 'a' "all" "include hidden files" <*> strArg "FILEPATH"

ls :: Bool -> FilePath -> IO ()
ls hidden fp = sort <$>
  (if hidden then getDirectoryContents else listDirectory) fp >>= mapM_ putStrLn
