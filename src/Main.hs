module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import LexBF
import ParBF
import AbsBF
import ErrM

import CodeGen
import LLVM

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hPutStrLn stderr "Usage: bfc <file.bf> [-o output] [-emit-llvm] [-S]"
      exitFailure
    (file:flags) -> do
      contents <- readFile file
      case pBF (myLexer contents) of
        Bad err -> do
          hPutStrLn stderr $ "Parse error: " ++ err
          exitFailure
        Ok tree -> do
          let llvmMod = codegenModule tree
          processFlags file flags llvmMod
          exitSuccess

-- | Process command line flags
processFlags :: FilePath -> [String] -> AST.Module -> IO ()
processFlags inputFile flags mod
  | "-emit-llvm" `elem` flags = do
      let outputFile = getOutputFile flags inputFile ".ll"
      writeLLVMIR outputFile mod
      putStrLn $ "Generated LLVM IR: " ++ outputFile
  
  | "-S" `elem` flags = do
      let outputFile = getOutputFile flags inputFile ".s"
      putStrLn $ "Generating assembly: " ++ outputFile
      -- Would need LLVM's assembly generation here
      writeLLVMIR (replaceExtension outputFile ".ll") mod
  
  | otherwise = do
      let outputFile = getOutputFile flags inputFile ".bc"
      toBitcode outputFile mod
      putStrLn $ "Generated bitcode: " ++ outputFile

-- | Get output file name
getOutputFile :: [String] -> FilePath -> String -> FilePath
getOutputFile flags inputFile defaultExt =
  case getOutput flags of
    Just file -> file
    Nothing -> replaceExtension inputFile defaultExt

-- | Extract -o flag value
getOutput :: [String] -> Maybe FilePath
getOutput [] = Nothing
getOutput ("-o":file:_) = Just file
getOutput (_:rest) = getOutput rest

-- | Replace file extension
replaceExtension :: FilePath -> String -> FilePath
replaceExtension path newExt = 
  let base = takeWhile (/= '.') path
  in base ++ newExt
