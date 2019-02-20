module SimpleCmdArgs
  (simpleCmdArgs,
   strArg,
   Subcommand(..),
   subcommands
  )
where

import Control.Monad (join)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Data.Monoid (mconcat)
#endif
import Data.Semigroup ((<>))
import Data.Version
import Options.Applicative

-- | Parser executor for tool with subcommands
simpleCmdArgs
  :: Maybe Version
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser (IO ())
  -- ^ commands
  -> IO ()
simpleCmdArgs mversion h pd cmdsParser = join $
  customExecParser (prefs showHelpOnEmpty)
  (case mversion of
    (Just version) -> info (helper <*> versionOption version <*> cmdsParser) desc
    Nothing -> info (helper <*> cmdsParser) desc)
  where 
    desc = fullDesc <> header h <> progDesc pd

    versionOption ver =
      infoOption (showVersion ver) (long "version" <> help "Show version")

data Subcommand = Subcommand String String (Parser (IO ()))

subcommands :: [Subcommand] -> Parser (IO ())
subcommands = subparser . mconcat . map cmdToParse
  where
    cmdToParse (Subcommand name cmddesc cmdparse) =
      command name (info cmdparse (progDesc cmddesc))

strArg :: String -> Parser String
strArg var = strArgument (metavar var)
