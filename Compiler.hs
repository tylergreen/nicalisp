module Compiler where

import Data.List -- { intersperse }

-- AST
data Exp = StrConst String 
         | IntConst Integer
         | Prim String
         | Var String  -- Symbols
         | Lambda [Exp] Exp -- what is the the second exp?  begin or sequence?
         | If Exp Exp Exp
         | Set Exp Exp 
         | Let [(Exp,Exp)] Exp
         | LetRec [(Exp, Exp)] Exp -- (letrec ((<var> (lambda (<var>...) <exp>))) <exp>)
         | Begin [Exp]
         | Seq [Exp] -- List
           deriving Show

-- Constants
javac :: Exp -> String           
javac (IntConst val) = concat ["new IntValue(" , show val , ")"]
javac (StrConst s) = concat ["\"", s , "\"" ]
-- javac (StrConst str) = concat [

-- Primitives 

javac (Prim "+") = "sum"
javac (Prim "-") = "difference"
javac (Prim "*") = "product"
javac (Prim "=") = "numEqual"
javac (Prim "display") = "display"
javac (Prim _) = error "unhandled primitive"

javac var@(Var _) = if mutable var
                   then concat ["m_", mangle var, ".value" ]
                   else mangle var


-- javac (Let (vars, exps) body) = javac (Seq [Lambda vars body, exps])


{- side note: code generation
 is extremely similar to html templating.  Want a nice code
 templating/generation language . this concat [ ] is not slick enough for my tastes.
 Want haskell code generation to reflect the look/code style of the target language.
-}

javac (Lambda params body) = unlines [ "new NullProcValue" ++ 
                                       show (length params) ++ "() {",
                                    " public Value apply (",
                                    java_compile_params params ++ ") {",
                                    concatMap wrap_mutable (filter mutable params),
                                    "   return " ++ javac body ++  " ; ",
                                    "} } " ]

javac exp@(Let _ _) = javac $ let_lambda exp
javac exp@(LetRec [(_,_)] _) = javac $ letrec1_y exp

-- not sure what the name of the "x" var should really be
javac (Begin exps)
    = javac $ dummy_bind exps
      where dummy_bind [x] = x
            dummy_bind (x:xs) = Let [(Var "x", x)] (dummy_bind xs)

-- Application
-- only handles the one case 
javac (Seq (f:args)) = concat [ "((ProcValue",
                                     show (length args),
                                     ")(",
                                     javac f, 
                                     ")).apply(",
                                      concat (intersperse ", " (map javac args)),
                                     ")" ]

javac (If pred conseq alt) = concat [ "(", javac pred, ").toBoolean() ? (",
                                         javac conseq, ") : ",
                                         javac alt, ")" ]

javac (Set var exp) = concat [ "VoidValue.Void(m_",
                              mangle var, ".value = ",
                              javac exp, ")" ]
                                                              

mutable :: Exp -> Bool
mutable _ = False  -- no mutables for now

wrap_mutable :: Exp -> String
wrap_mutable x = "final ValueCell m_" ++ (mangle x) ++ " = new ValueCell(" ++  (mangle x) ++ ")"

java_compile_params :: [Exp] -> String
java_compile_params params = concat $ intersperse ", " 
                               $ map (( "final Value " ++) . mangle) params

-- used legal java string for now
mangle :: Exp -> String
mangle (Var name) = name
-- mangle _ = error

-- Recursion

xargs :: Int -> [Exp]
xargs n = take n $ map (Var . ("x" ++) . show) [1..]

fix f = f (fix f)
fix2 f = f $ fix2 f

fixn n = Seq [f, f]
    where f = Lambda [Var "h"] 
                     $ Lambda [Var "F"]
                            (Seq [Var "F", Lambda (xargs n)
                                              (Seq
                                               ([Seq
                                                [Seq [Var "h", Var "h"],
                                                     Var "F" ]] ++ xargs n ))])
                         
-- Macro Expansions -- aka (Desugar)
-- aka let=>lambda
let_lambda :: Exp -> Exp
let_lambda (Let bindings body) = 
                         Seq $ (Lambda (map fst bindings) body)
                                      : map snd bindings

letrec1_y (LetRec [(name,f@(Lambda params _))] body)
               = Let [(name,
                        Seq [fixn (length params), Lambda [name] f])]
                     body

letrec1_y exp@(LetRec (x:xs) _) = exp

java_compile_program exp
    = concat ["public class BOut extends RuntimeEnvironment {",
                "public static void main (String[] args ) { ",
                    javac exp , ";",
                              "}",
                         "}"
             ]



