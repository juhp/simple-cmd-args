# simple-cmd-args

[![Hackage](https://img.shields.io/hackage/v/simple-cmd-args.svg)](https://hackage.haskell.org/package/simple-cmd-args)
[![BSD license](https://img.shields.io/badge/license-BSD-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/simple-cmd-args/badge/lts)](http://stackage.org/lts/package/simple-cmd-args)
[![Stackage Nightly](http://stackage.org/package/simple-cmd-args/badge/nightly)](http://stackage.org/nightly/package/simple-cmd-args)
[![Build Status](https://travis-ci.com/juhp/simple-cmd-args.svg?branch=master)](https://travis-ci.com/juhp/simple-cmd-args)

A thin layer over optparse-applicative that avoids type plumbing for
subcommands by using `Parser (IO ())`.

## Usage

```console
$ cat readme.hs
```
```haskell
import SimpleCmdArgs
import Control.Applicative (some)
import System.Directory

main =
  simpleCmdArgs Nothing "readme example" "Longer description..." $
  subcommands
    [ Subcommand "echo" "Print words" $
      putStrLn . unwords <$> some (strArg "STR...")
    , Subcommand "ls" "List directory" $
      ls <$> strArg "DIR"
    , Subcommand "mkdir" "Create directory" $
      mkdir <$> parentsOpt <*> strArg "DIR"
    ]
  where
    parentsOpt = switchWith 'p' "parents" "Make missing directories"

ls :: FilePath -> IO ()
ls dir =
  listDirectory dir >>= mapM_ putStrLn

mkdir :: Bool -> FilePath -> IO ()
mkdir parents =
  if parents then createDirectoryIfMissing True else createDirectory
```
```console
$ ghc readme.hs
$ ./readme --help
readme example

Usage: readme COMMAND
  Longer description...

Available options:
  -h,--help                Show this help text

Available commands:
  echo                     Print words
  ls                       List directory
  mkdir                    Create directory
$ ./readme echo hello world
hello world
```

See more [examples](https://github.com/juhp/simple-cmd-args/tree/master/examples).
