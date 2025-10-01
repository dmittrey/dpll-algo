module Main (main) where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import Solver (satSolve)

import Language.CNF.Parse.ParseDIMACS

main :: IO ()
main = do
  args <- getArgs
  case args of
      [file] -> do
          result <- parseFile file
          case result of
            Left err ->
              putStrLn $ "Error: " ++ show err
            Right cnf -> do
              putStrLn $ show (satSolve cnf)
      _ -> do
          hPutStrLn stderr "Usage: stella <SourceFile>"
          exitFailure
