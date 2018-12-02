{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import System.FilePath
import System.Process
import Distribution.System
import Data.Maybe
import Data.List
import Data.Char
import Data.Bool
import Text.Read
import qualified Text.ParserCombinators.ReadP (string)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

sequencePair :: (IO a, IO b) -> IO (a, b)
sequencePair (xm, ym) = xm >>= ((<$> ym) . (,))

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
        bitSize = case arch of
                    I386   -> B32
                    X86_64 -> B64

transformIdent :: Format -> String -> String
transformIdent (Elf _)   = id
transformIdent (Macho _) = ("_" ++)
transformIdent (Win _)   = ("_" ++)

data MaybeOptions = MaybeOptions
  { mOptInput          :: [(String, IO String)]
  , mOptOutput         :: Maybe FilePath
  , mOptOutputF        :: Maybe Format
  }

data Options = Options
  { optInput          :: [(String, IO String)]
  , optOutput         :: FilePath
  , optOutputF        :: Format
  }

initialOptions :: MaybeOptions
initialOptions = MaybeOptions
  { mOptInput          = []
  , mOptOutput         = Nothing
  , mOptOutputF        = Nothing
  }

fillDefaultOptions :: MaybeOptions -> Options
fillDefaultOptions opts = Options
  { optInput          = input
  , optOutput         = fromMaybe (intercalate "_" . map fst $ input) (mOptOutput opts)
  , optOutputF        = fromMaybe (nativeFormat buildPlatform) (mOptOutputF opts)
  }
  where input = reverse . mOptInput $ opts

inputOpt :: String -> MaybeOptions -> IO MaybeOptions
inputOpt arg opt = return opt { mOptInput = (takeBaseName arg, readFile arg) : mOptInput opt }

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
          exitWith ExitSuccess))
      "Show help"
  ]

getActions :: ([MaybeOptions -> IO MaybeOptions], [String], [String]) -> IO [MaybeOptions -> IO MaybeOptions]
getActions (actions, nonOptions, errors) = (sequence $ map errorWithoutStackTrace errors) >> return actions

assertInput :: MaybeOptions -> MaybeOptions
assertInput opts = bool opts (errorWithoutStackTrace "No input files given") . null . mOptInput $ opts

compileString :: (String, String) -> String
compileString (ident, str) = "global "
                          ++ ident
                          ++ "\n"
                          ++ ident
                          ++ " DB "
                          ++ (intercalate "," . map (show . fromEnum) $ str)
                          ++ ",0"

compile :: Format -> [(String, String)] -> String
compile format xs = intercalate "\n" . map ($ map (mapFst (transformIdent format)) xs) $
  [ const "section .data"
  , intercalate "\n" . map (compileString $)
  ]

generateCDecl :: (String, String) -> String
generateCDecl (ident, str) = "extern const char "
                          ++ ident
                          ++ "[];"

generateHeader :: Format -> [(String, String)] -> String
generateHeader _ xs = intercalate "\n" . map (generateCDecl $) $ xs

outputFiles :: [(String, Format -> [(String, String)] -> String)]
outputFiles =
  [ ("asm", compile)
  , ("h", generateHeader)
  ]

chain :: IO a -> [a -> IO ()] -> IO ()
chain x = foldl (>>) (return ()) . map (x >>=)

generateFiles :: [(String, Format -> [(String, String)] -> String)] -> Options -> IO ()
generateFiles files opts =
    chain (sequence . map (sequencePair . mapFst return) . optInput $ opts)
  . map
    (\(ext, gen) ->
        writeFile (optOutput opts ++ "." ++ ext)
      . gen (optOutputF opts)
    )
  $ files

passthrough :: (a -> IO ()) -> a -> IO a
passthrough f x = f x >> return x

main = getArgs                                          -- Get the arguments
   >>= return . getOpt (ReturnInOrder inputOpt) options -- Parse options to get a list of actions and errors
   >>= getActions                                       -- Throw the errors to get the actions
   >>= foldl (>>=) (return initialOptions)              -- Here we thread defaultOptions through all supplied option actions
   >>= return . assertInput                             -- Verify that input is non-empty
   >>= return . fillDefaultOptions
   >>= passthrough (generateFiles outputFiles)
   >>= (\opts ->
         callProcess "nasm" ["-f", show . optOutputF $ opts, optOutput opts ++ ".asm"]
       )

