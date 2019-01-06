import SimpleCmd (cmd_)
import SimpleCmdArgs

import Data.Version
import System.Directory (listDirectory)
import Options.Applicative (some)

main =
  toolWithCommands (makeVersion [0,1]) "An example cli tool" "Longer description..."
    [ Subcommand "hello" (pure (putStrLn "hello")) "Print hello"
    , Subcommand "list" (ls <$> strArg "DIR") "List DIR"
    , Subcommand "touch" (cmd_ "touch" <$> some (strArg "FILE")) "Touch FILE"
    ]

ls :: FilePath -> IO ()
ls fp = listDirectory fp >>= mapM_ putStrLn
