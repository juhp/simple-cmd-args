module SimpleCmdArgs
  (strArg,
   Subcommand(..),
   toolWithCommands)
where

import Control.Monad (join)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Data.Monoid (mconcat)
#endif
import Data.Semigroup ((<>))
import Data.Version
import Options.Applicative

data Subcommand = Subcommand String (Parser (IO ())) String

-- | Parser executor for tool with subcommands
toolWithCommands
  :: Maybe Version
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> [Subcommand]
  -- ^ commands
  -> IO ()
toolWithCommands mversion h pd cmds = join $
  customExecParser (prefs showHelpOnEmpty)
  (case mversion of
    (Just version) -> info (helper <*> versionOption version <*> cmdsParser) desc
    Nothing -> info (helper <*> cmdsParser) desc)
  where 
    desc = fullDesc <> header h <> progDesc pd

    versionOption ver =
      infoOption (showVersion ver) (long "version" <> help "Show version")

    cmdsParser :: Parser (IO ())
    cmdsParser = subparser $ mconcat $ map cmdToParse cmds

    cmdToParse (Subcommand name cmdparse cmddesc) = 
      command name (info cmdparse (progDesc cmddesc))

strArg :: String -> Parser String
strArg var = strArgument (metavar var)
