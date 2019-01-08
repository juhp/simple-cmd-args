import SimpleCmdArgs
import Control.Applicative (some)
import SimpleCmd (cmd_)

main =
  toolWithCommands Nothing "my example tool" "Longer description..."
    [ Subcommand "echo" (putStrLn <$> strArg "NAME") "Print name"
    , Subcommand "ls" (cmd_ "ls" <$> some (strArg "FILE...")) "Touch FILE"
    ]
