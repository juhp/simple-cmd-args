import SimpleCmd (cmd_)
import SimpleCmdArgs

import Control.Applicative (some)
import Data.Version (makeVersion)
import System.Directory (listDirectory)

main =
  simpleCmdArgs (Just ver) "example-cli" "An example of subcommands..." $
  subcommands
  [ Subcommand "hello" "Print hello" $ pure (putStrLn "hello")
  , Subcommand "list" "List DIR" $ ls <$> strArg "DIR"
  , Subcommand "touch" "Touch files" $ cmd_ "touch" <$> some (strArg "FILE...")
  ]
  where
    ver = makeVersion [0,1]

ls :: FilePath -> IO ()
ls fp = listDirectory fp >>= mapM_ putStrLn
