import SimpleCmdArgs
import Control.Applicative (some)
import SimpleCmd (cmd_)

main =
  simpleCmdArgs Nothing "my example tool" "Longer description..." $
  subcommands
    [ Subcommand "echo" (putStrLn <$> strArg "NAME") "Print name"
    , Subcommand "ls" (cmd_ "ls" <$> some (strArg "FILE...")) "Touch FILE"
    ]
