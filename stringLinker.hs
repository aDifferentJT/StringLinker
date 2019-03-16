{-# LANGUAGE RankNTypes, RecordWildCards #-}

import Control.Arrow (first)
import Distribution.System
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import System.Console.GetOpt (OptDescr (Option), getOpt, ArgDescr(ReqArg, NoArg), ArgOrder (ReturnInOrder), usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)
import Text.Read (ReadPrec, readPrec, (+++), lift)
import qualified Text.ParserCombinators.ReadP (string)

string :: String -> ReadPrec String
string = lift . Text.ParserCombinators.ReadP.string

data Bits = B32
          | B64
  deriving Eq

instance Show Bits where
  show B32 = "32"
  show B64 = "64"

instance Read Bits where
  readPrec = (string "32" >> return B32)
         +++ (string "64" >> return B64)

data Format = Elf   Bits
            | Macho Bits
            | Win   Bits
  deriving Eq

instance Show Format where
  show (Elf   b) = "elf"   ++ show b
  show (Macho b) = "macho" ++ show b
  show (Win   b) = "win"   ++ show b

instance Read Format where
  readPrec = (string "elf"   >> (Elf   <$> readPrec))
         +++ (string "macho" >> (Macho <$> readPrec))
         +++ (string "win"   >> (Win   <$> readPrec))

nativeFormat :: Platform -> Format
nativeFormat (Platform arch os) = objF bitSize
  where objF = case os of
                 Linux   -> Elf
                 OSX     -> Macho
                 Windows -> Win
                 _       -> errorWithoutStackTrace "Unsupported OS"
        bitSize = case arch of
                    I386   -> B32
                    X86_64 -> B64
                    _      -> errorWithoutStackTrace "Unsupported Architecture"

transformIdent :: Format -> String -> String
transformIdent (Elf _)   = id
transformIdent (Macho _) = ("_" ++)
transformIdent (Win _)   = ("_" ++)

data MaybeOptions = MaybeOptions
  { mOptInputs         :: [(String, String)]
  , mOptOutput         :: Maybe FilePath
  , mOptOutputF        :: Maybe Format
  }

data Options = Options
  { optInputs         :: [(String, String)]
  , optOutput         :: FilePath
  , optOutputF        :: Format
  }

initialOptions :: MaybeOptions
initialOptions = MaybeOptions
  { mOptInputs         = []
  , mOptOutput         = Nothing
  , mOptOutputF        = Nothing
  }

fillDefaultOptions :: MaybeOptions -> Options
fillDefaultOptions MaybeOptions{..} = Options
  { optInputs         = inputs
  , optOutput         = fromMaybe (intercalate "_" . map fst $ inputs) mOptOutput
  , optOutputF        = fromMaybe (nativeFormat buildPlatform) mOptOutputF
  }
  where inputs = reverse mOptInputs

inputOpt :: String -> MaybeOptions -> IO MaybeOptions
inputOpt arg MaybeOptions{..} = (\file -> MaybeOptions{ mOptInputs = (takeBaseName arg, file) : mOptInputs, .. }) <$> readFile arg

options :: [OptDescr (MaybeOptions -> IO MaybeOptions)]
options =
  [ Option
      "i"
      ["input"]
      (ReqArg
        inputOpt
        "FILE")
      "Input file"
  , Option
      "o"
      ["output"]
      (ReqArg
        (\arg opt -> return opt { mOptOutput = Just arg })
        "FILE")
      "Output file"
  , Option
      "f"
      ["format"]
      (ReqArg
        (\arg opt -> return opt { mOptOutputF = Just . read $ arg })
        "FORMAT")
      "Set output format"
  , Option
      "h"
      ["help"]
      (NoArg
        (\_ -> do
          prg <- getProgName
          hPutStrLn stderr (usageInfo prg options)
          exitSuccess))
      "Show help"
  ]

getActions :: ([MaybeOptions -> IO MaybeOptions], [String], [String]) -> IO [MaybeOptions -> IO MaybeOptions]
getActions (actions, nonOptions, errors) = do
  mapM_ errorWithoutStackTrace errors
  bool (errorWithoutStackTrace ("Unconsumed options: " ++ intercalate ", " nonOptions)) (return actions) (null nonOptions)

assertInput :: MaybeOptions -> MaybeOptions
assertInput MaybeOptions{..} = bool MaybeOptions{..} (errorWithoutStackTrace "No input files given") . null $ mOptInputs

compileString :: (String, String) -> String
compileString (ident, str) = concat
  [ "global "
  , ident
  , "\n"
  , ident
  , " DB "
  , intercalate "," . map (show . fromEnum) $ str
  , ",0"
  ]

compile :: Format -> [(String, String)] -> String
compile format xs = intercalate "\n" $ "section .data" : map compileString idents
  where idents = map (first (transformIdent format)) xs

generateCDecl :: (String, String) -> String
generateCDecl (ident, _) = "extern const char " ++ ident ++ "[];"

generateHeader :: Format -> [(String, String)] -> String
generateHeader _ = intercalate "\n" . map generateCDecl

outputFiles :: [(String, Format -> [(String, String)] -> String)]
outputFiles =
  [ ("asm", compile)
  , ("h", generateHeader)
  ]

generateFiles :: Options -> [(String, Format -> [(String, String)] -> String)] -> IO ()
generateFiles Options{..} =
  mapM_ $ \(ext, gen) -> writeFile (optOutput ++ "." ++ ext) . gen optOutputF $ optInputs

main :: IO ()
main = do
  Options{..} <- fillDefaultOptions . assertInput <$> (
        getOpt (ReturnInOrder inputOpt) options <$> getArgs -- Parse options to get a list of actions and errors
    >>= getActions                                          -- Throw the errors to get the actions
    >>= foldl (>>=) (return initialOptions)                 -- Here we thread defaultOptions through all supplied option actions
    )

  generateFiles Options{..} outputFiles
  callProcess "nasm" ["-f", show optOutputF, optOutput ++ ".asm"]

