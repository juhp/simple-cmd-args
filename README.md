# simple-cmd-args

[![Hackage](https://img.shields.io/hackage/v/simple-cmd-args.svg)](https://hackage.haskell.org/package/simple-cmd-args)
[![BSD license](https://img.shields.io/badge/license-BSD-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/simple-cmd-args/badge/lts)](http://stackage.org/lts/package/simple-cmd-args)
[![Stackage Nightly](http://stackage.org/package/simple-cmd-args/badge/nightly)](http://stackage.org/nightly/package/simple-cmd-args)
[![Build status](https://secure.travis-ci.org/juhp/simple-cmd-args.svg)](https://travis-ci.org/juhp/simple-cmd-args)

A thin layer over optparse-applicative that avoids type plumbing for
commands by using `Parser (IO ())`. It also supports subcommands.

## Usage

```haskell
import SimpleCmdArgs
import Control.Applicative (some)
import SimpleCmd (cmd_)

main =
  simpleCmdArgs Nothing "my example tool" "Longer description..." $
  subcommands
    [ Subcommand "echo" (putStrLn <$> strArg "NAME") "Print name"
    , Subcommand "ls" (cmd_ "ls" <$> some (strArg "FILE...")) "Touch FILE"
    ]
```

See more [examples](https://github.com/juhp/simple-cmd-args/tree/master/examples).
