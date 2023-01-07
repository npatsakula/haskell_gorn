module Main where

import Tree qualified
import Duplicates qualified

import Options.Applicative

data Opt = Opt
    { cmd :: Command
    , source :: FilePath
    , output :: Maybe FilePath
    , threads :: Int
    , verbose :: Bool
    }

data Command
    = Build
    | Duplicates

parser :: Parser Opt
parser = Opt
    <$> subparser
        (  command "build" (info (pure Build) (progDesc "Build hash tree."))
        <> command "duplicates" (info (pure Duplicates) (progDesc "Find duplicates.")) )
    <*> strOption
        (  short 'i'
        <> long "input"
        <> metavar "PATH"
        <> help "Input directory path." )
    <*> option auto
        (  short 'o'
        <> long "output"
        <> metavar "PATH"
        <> value Nothing
        <> help "Output path. STDIN if none." )
    <*> option auto
        (  short 't'
        <> long "threads"
        <> metavar "INT"
        <> value 4
        <> help "Threads count." )
    <*> switch
         (  short 'v'
         <> long "verbose"
         <> help "Verbose error output." )

main :: IO ()
main = do
    Opt c s o t v <- execParser $ info (parser <**> helper) ( fullDesc
        <> progDesc "FS duplicate finder."
      )
    (errors, tree) <- Tree.buildTree t s
    when v $ print errors
    let writer = maybe putStrLn writeFile o
    case c of
        Build -> writer $ Tree.printTree tree
        Duplicates -> do
            let map = Duplicates.buildMap mempty tree
            writer $ Duplicates.printMap $ Duplicates.filterMap map
