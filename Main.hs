module Main where

import Compiler
import Parser
import Text.ParserCombinators.Parsec --hiding (spaces)

compile_scm exp = case (parse pExp "" exp) of
                 Left err ->  show err
                 Right val -> java_compile_program val