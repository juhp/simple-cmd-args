import SimpleCmdArgs
import Control.Applicative (some)
import System.Directory

main =
  simpleCmdArgs Nothing "example-tool" "Longer description..." $
  subcommands
    [ Subcommand "echo" "Print words" $
      putStrLn . unwords <$> some (strArg "STR...")
    , Subcommand "ls" "List directory" $
      ls <$> strArg "DIR"
    , Subcommand "mkdir" "Create directory" $
      mkdir <$> parentsOpt <*> strArg "DIR"
    ]
  where
    parentsOpt = switchWith 'p' "parents" "Make missing directories"

ls :: FilePath -> IO ()
ls dir =
  listDirectory dir >>= mapM_ putStrLn

mkdir :: Bool -> FilePath -> IO ()
mkdir parents =
  if parents then createDirectoryIfMissing True else createDirectory
