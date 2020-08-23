module Main where

import Parser
import Text.Parsec
import Html
import System.Environment

main :: IO ()
main = do
  (filename:outputFilename:_) <- getArgs
  cnts <- readFile filename
  case (runParser parserMarkdown 0 filename cnts) of
    Left err -> putStrLn . show $ err
    Right markdown -> let html = genHtml markdown
                      in do
                        writeFile outputFilename html
                        putStrLn . show $ markdown