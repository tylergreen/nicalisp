module Main where

import Compiler
import Parser
import Text.ParserCombinators.Parsec --hiding (spaces)

java_run exp = case (parse pExp "" exp) of
                 Left err ->  show err
                 Right val -> java_compile_program val