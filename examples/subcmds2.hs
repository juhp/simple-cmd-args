import SimpleCmdArgs

main =
  simpleCmdArgs Nothing "subcmds2" "Another set of subcommands" $
  subcommands
  [ Subcommand "noargs" "takes no args" (pure (return ()))
  , Subcommand "onearg" "takes an arg" (putStrLn <$> strArg "STR")
  , Subcommand "oneopt" "takes an opt" $
    print <$> switchWith 's' "switch" "Some switch"
  , Subcommand "optional" "has optional" $
    print <$> optionalWith auto 'n' "number" "INT" "optional number" 0
  ]
