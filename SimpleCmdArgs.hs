{-|
This library provides a thin layer on optparse-applicative
argument and option parsing, using @Parser (IO ())@,
applying commands directly to their argument parsing.

A few option Mod functions are also provided.
-}

module SimpleCmdArgs
  (simpleCmdArgs,
   simpleCmdArgs',
   Subcommand(..),
   subcommands,
   strArg,
   switchWith,
   switchMods,
   strOptionWith,
   optionWith,
   optionMods,
   strOptionalWith,
   optionalWith,
   optionalMods,
   argumentWith
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

-- | Parser executor (allows interspersed args and options)
--
-- > simpleCmdArgs (Just version) "summary" "program description" $ myCommand <$> myOptParser <*> myargsParser
simpleCmdArgs ::
  Maybe Version
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser (IO ())
  -- ^ commands
  -> IO ()
simpleCmdArgs mversion h pd =
  simpleCmdArgsWithMods mods mversion
  where
    mods = fullDesc <> header h <> progDesc pd

-- | Parser executor without interspersing options and args
--
-- > simpleCmdArgs' Nothing "summary" "program description" $ myCommand <$> myOptParser <*> myargsParser
simpleCmdArgs'
  :: Maybe Version
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser (IO ())
  -- ^ commands
  -> IO ()
simpleCmdArgs' mversion h pd =
  simpleCmdArgsWithMods mods mversion
  where
    mods = fullDesc <> header h <> progDesc pd <> noIntersperse

-- | Generic parser executor with explicit info modifiers
simpleCmdArgsWithMods ::
  InfoMod (IO ()) -- ^ modifiers
  -> Maybe Version -- ^ version string
  -> Parser (IO ()) -- ^ commands
  -> IO ()
simpleCmdArgsWithMods mods mversion cmdsParser = join $
  customExecParser (prefs showHelpOnEmpty)
  (case mversion of
    (Just version) -> info (helper <*> versionOption version <*> cmdsParser) mods
    Nothing -> info (helper <*> cmdsParser) mods)
  where 
    versionOption ver =
      infoOption (showVersion ver) (long "version" <> help "Show version")

-- | > Subcommand "command" "help description text" $ myCommand <$> optParser
data Subcommand =
  Subcommand String String (Parser (IO ()))

-- | list of @Subcommand@ that can be run by @simpleCmdArgs@
subcommands :: [Subcommand] -> Parser (IO ())
subcommands = subparser . mconcat . map cmdToParse
  where
    cmdToParse (Subcommand name cmddesc cmdparse) =
      command name (info cmdparse (progDesc cmddesc))

-- | A string arg parser with a METAVAR for help
strArg :: String -> Parser String
strArg var = strArgument (metavar var)

-- | switch with Mods
--
-- > switchWith 'o' "option" "help description"
switchWith :: Char -> String -> String -> Parser Bool
switchWith s l h =
  switch (switchMods s l h)

-- | @Mod@s for a switch.
--
-- > switchMods 'o' "option" "help description"
switchMods :: HasName f =>
  Char -> String -> String -> Mod f a
switchMods s l h =
  short s <> long l <> help h

-- | strOption with Mods
--
-- > strOptionWith 'o' "option" "METAVAR" "help description"
strOptionWith :: Char -> String -> String -> String -> Parser String
strOptionWith s l meta h =
  strOption (optionMods s l meta h)

-- | option with Mods
--
-- > optionWith 'o' "option" "METAVAR" "help description"
optionWith :: ReadM a -> Char -> String -> String -> String -> Parser a
optionWith r s l meta h =
  option r (optionMods s l meta h)

-- | @Mod@s for a mandatory option.
--
-- > optionMods 'o' "option" "METAVAR" "help description"
optionMods :: (HasMetavar f, HasName f) =>
  Char -> String -> String -> String -> Mod f a
optionMods s l meta h =
  short s <> long l <> metavar meta <> help h

-- | strOptional with Mods
--
-- > strOptionalWith 'o' "option" "METAVAR" "help description" default
strOptionalWith :: Char -> String -> String -> String -> String -> Parser String
strOptionalWith s l meta h d =
  strOption (optionalMods s l meta h d)

-- | optional option with Mods, includes a default value.
--
-- > optionalWith 'o' "option" "METAVAR" "help description" default
optionalWith :: ReadM a -> Char -> String -> String -> String -> a -> Parser a
optionalWith r s l meta h d =
  option r (optionalMods s l meta h d)

-- | @Mod@s for an optional option: includes a default value.
--
-- > optionalMods 'o' "option" "METAVAR" "help description" default
optionalMods :: (HasMetavar f, HasName f, HasValue f) =>
  Char -> String -> String -> String -> a -> Mod f a
optionalMods s l meta h d =
  short s <> long l <> metavar meta <> help h <> value d

-- | argument with METAVAR
--
-- > argumentWith auto "METAVAR"
argumentWith :: ReadM a -> String -> Parser a
argumentWith r meta =
  argument r (metavar meta)
