module SimpleCmdArgs
  (emptyVersion,
   strArg,
   Subcommand(..),
   toolWithCommands)
where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Version
import Control.Monad (join)

data Subcommand = Subcommand String (Parser (IO ())) String

-- | Parser executor for tool with subcommands
toolWithCommands
  :: Version
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> [Subcommand]
  -- ^ commands
  -> IO ()
toolWithCommands version h pd cmds = join $ 
  customExecParser (prefs showHelpOnEmpty)
  (info (helper <*> versionOption <*> cmdsParser) desc)
  where 
    desc = fullDesc <> header h <> progDesc pd

    versionOption =
      infoOption (showVersion version) (long "version" <> help "Show version")

    cmdsParser :: Parser (IO ())
    cmdsParser = subparser $ mconcat $ map cmdToParse cmds

    cmdToParse (Subcommand name cmdparse cmddesc) = 
      command name (info cmdparse (progDesc cmddesc))

strArg :: String -> Parser String
strArg var = strArgument (metavar var)

emptyVersion :: Version
emptyVersion = makeVersion []
