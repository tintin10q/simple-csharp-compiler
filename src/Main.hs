module Main where

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode
import Prelude hiding ((<$), (<*), (*>))
import Data.Either (fromRight, fromLeft, isRight)


main :: IO ()
main = do
    -- get command line arguments
    args  <- getArgs
    files <- case args of
              []  ->  do
                putStrLn "no argument given; assuming example.cs"
                return ["example.cs"]
              xs  ->  return xs
    -- translate each of the files
    processFiles files

processFiles :: [FilePath] -> IO ()
processFiles = mapM_ $ processFile
        . \f -> (f, addExtension (dropExtension f) "ssm")


-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process = formatCode
                . errorCheck
                . foldCSharp codeAlgebra
                . run (pClass <* eof)
                . run lexicalScanner
        errorCheck c = case c of
                          (Left (Error name)) -> error name
                          (Right code) -> code

run :: Parser s a -> [s] -> a
run p = fst . head . filter (null . snd) . parse p
