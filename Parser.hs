module Parser where

import Text.ParserCombinators.Parsec --hiding (spaces)
import Text.ParserCombinators.Parsec.Token --hiding (spaces)
import Monad
import Compiler


pExp :: Parser Exp
pExp = pList <|>
       pAtom 

pAtom = pInt <|>
        pVar <|> 
        pStr <|>
        pPrim

pList = do char '('
           spaces
           exp <- pApp
           spaces
           char ')'
           return $ exp

pApp = pLambda <|>
       pIf <|> 
       pSet <|>
       pLet <|>
       pLetRec <|>
       pBegin <|>
       pSeq 
       

pInt = liftM (IntConst . read) $ many1 digit

pStr = do char '"'
          s <- many (noneOf "\"")
          char '"'
          return $ StrConst s

-- enforce java rules here so no mangling needed?
pVar = do first <- letter
          rest <- many (letter <|> digit)
          return $ Var (first : rest)

{-
pInt = do n <- many1 digit
          return $ read  n ::Int

-}

pPrim =  do 
  p <- (string "+") <|> (string "display")
  return $ Prim p


pSeq = do exps <- sepBy pExp spaces
          return $ Seq exps

pLambda = do string "fn"
             spaces
             (Seq args) <- pList -- want good error message if not Seq
             spaces
             body <- pExp
             return $ Lambda args body

pIf  = do string "if"
          spaces
          pred <- pExp
          spaces
          conseq <- pExp
          spaces
          alt <- pExp
          return $ If pred conseq alt

pSet = do string "set!"
          spaces
          var <- pVar
          spaces 
          value <- pExp
          return $ Set var value

pLet = do string "let"
          spaces 
          char '('
          spaces
          bindings <- sepBy pBind spaces
          spaces
          char ')'
          spaces 
          body <- pExp
          return $ Let bindings body

pBind = do char '('
           spaces
           var <- pExp 
           spaces
           val <- pExp
           spaces
           char ')'
           return (var, val)
                

pLetRec = do string "letrec"
             spaces 
             char '('
             spaces
             bindings <- many pBind
             spaces
             char ')'
             spaces 
             body <- pExp
             return $ LetRec bindings body

pBegin = do string "begin"
            (Seq exps) <- pSeq
            return $ Begin exps
            
                    
          

          

