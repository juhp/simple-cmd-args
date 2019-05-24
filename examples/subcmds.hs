import SimpleCmd (cmd_)
import SimpleCmdArgs

import Control.Applicative (some)
import Data.Version (makeVersion)
import System.Directory (listDirectory)

main =
  simpleCmdArgs (Just ver) "An example cli tool" "Longer description..." $
  subcommands
  [ Subcommand "hello" "Print hello" $ pure (putStrLn "hello")
  , Subcommand "list" "List DIR" $ ls <$> strArg "DIR"
  , Subcommand "touch" "Touch FILE" $ cmd_ "touch" <$> some (strArg "FILE")
  ]
  where
    ver = makeVersion [0,1]

ls :: FilePath -> IO ()
ls fp = listDirectory fp >>= mapM_ putStrLn
