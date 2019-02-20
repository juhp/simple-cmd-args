import SimpleCmdArgs
import Control.Applicative (some)
import SimpleCmd (cmd_)

main =
  simpleCmdArgs Nothing "my example tool" "Longer description..." $
  subcommands
    [ Subcommand "echo" "Print name" $
      putStrLn <$> strArg "NAME"
    , Subcommand "ls" "Touch FILE" $
      cmd_ "ls" <$> some (strArg "FILE...")
    ]
