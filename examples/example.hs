import SimpleCmd (cmd_)
import SimpleCmdArgs

import Data.Version (makeVersion)
import System.Directory (listDirectory)
import Options.Applicative (some)

main =
  simpleCmdArgs (Just ver) "An example cli tool" "Longer description..." $
  subcommands
  [ Subcommand "hello" (pure (putStrLn "hello")) "Print hello"
  , Subcommand "list" (ls <$> strArg "DIR") "List DIR"
  , Subcommand "touch" (cmd_ "touch" <$> some (strArg "FILE")) "Touch FILE"
  ]
  where
    ver = makeVersion [0,1]

ls :: FilePath -> IO ()
ls fp = listDirectory fp >>= mapM_ putStrLn
