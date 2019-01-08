# simple-cmd-args

[![Hackage](https://img.shields.io/hackage/v/simple-cmd-args.svg)](https://hackage.haskell.org/package/simple-cmd-args)
[![BSD license](https://img.shields.io/badge/license-BSD-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/simple-cmd-args/badge/lts)](http://stackage.org/lts/package/simple-cmd-args)
[![Stackage Nightly](http://stackage.org/package/simple-cmd-args/badge/nightly)](http://stackage.org/nightly/package/simple-cmd-args)
[![Build status](https://secure.travis-ci.org/juhp/simple-cmd-args.svg)](https://travis-ci.org/juhp/simple-cmd-args)

A thin layer over optparse-applicative that avoids type plumbing for
the common use case of a commandline tool with subcommands,
by using `Parser (IO ())`

## Example

See [https://github.com/juhp/simple-cmd-args/tree/master/examples](examples/)

## Usage

```haskell
import SimpleCmdArgs
import Control.Applicative (some)
import SimpleCmd (cmd_)

main =
  toolWithCommands Nothing "my example tool" "Longer description..."
    [ Subcommand "echo" (putStrLn <$> strArg "NAME") "Print name"
    , Subcommand "ls" (cmd_ "ls" <$> some (strArg "FILE...")) "Touch FILE"
    ]
```
