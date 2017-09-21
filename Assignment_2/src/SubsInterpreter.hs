module SubsInterpreter
       (
         Value(..),
         runExpr
       )
       where

import SubsAst

import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value -- variable aka symb table
type Primitive = [Value] -> Either Error Value  -- preexisting func
type PEnv = Map FunName Primitive  -- primitive env
type Context = (Env, PEnv)


-- Monad definition

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap = liftM

instance Applicative SubsM where
  pure = return
  (<*>) = ap

instance Monad SubsM where
  return x = SubsM (\(e,_) -> Right (x,e))
  f >>= m = SubsM (\(e0,ep0) -> case runSubsM f (e0,ep0) of
                                  Left errMsg -> Left errMsg
                                  Right (a,e) -> runSubsM (m a) (e,ep0))
  fail s = SubsM (\_-> Left s)


-- Context set up

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", equality)
                       , ("<", less)
                       , ("+", add)
                       , ("*", multiply)
                       , ("-", minus)
                       , ("%", modulo)
                       , ("Array", mkArray)
                       ]

mkArray :: Primitive
mkArray [IntVal n]
  | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

add :: Primitive
add [IntVal a, IntVal b] = return $ IntVal (a + b)
add [StringVal a, StringVal b] = return $ StringVal (a ++ b)
add [IntVal a, StringVal b] = return $ StringVal (show a ++ b)
add [StringVal a, IntVal b] = return $ StringVal (a ++ show b)
add _ = Left "\"+\" applied to incompatible types, try any combination of String and Int"

minus :: Primitive
minus [IntVal a, IntVal b] = return $ IntVal (a - b)
minus _ = Left "\"-\" applied to incompatible types, try: Int - Int"

multiply :: Primitive
multiply [IntVal a, IntVal b] = return $ IntVal (a * b)
multiply _ = Left "\"*\" applied to incompatible types, try: Int * Int"

modulo :: Primitive
modulo [IntVal _, IntVal 0] = Left "\"% 0\" is undefined, try different value"
modulo [IntVal a, IntVal b] = return $ IntVal (a `mod` b)
modulo _ = Left "\"*\" applied to incompatible types, try: Int % Int"

equality::[Value] -> Either Error Value
equality [IntVal a ,IntVal b] = if a == b then Right TrueVal
                                  else Right FalseVal
equality [UndefinedVal,UndefinedVal] = Right TrueVal
equality [TrueVal,TrueVal] = Right TrueVal
equality [FalseVal,FalseVal] = Right TrueVal
equality [TrueVal,FalseVal] = Right FalseVal
equality [FalseVal,TrueVal] = Right FalseVal
equality [ArrayVal [], ArrayVal []] = Right TrueVal
equality [ArrayVal _, ArrayVal []] = Right FalseVal
equality [ArrayVal [], ArrayVal _] = Right FalseVal
equality [ArrayVal (h1:t1), ArrayVal (h2:t2)] = case equality [h1,h2] of
                                                      Right TrueVal -> equality [ArrayVal t1, ArrayVal t2]
                                                      Right FalseVal -> Right FalseVal
                                                      _ -> Right FalseVal

equality [StringVal s1,StringVal s2] = if s1 == s2 then Right TrueVal
                              else Right FalseVal
equality [_] = Left "Too few arguments"
equality [_, _] = Right FalseVal
equality _ = Left "Too many arguments"


less::[Value] -> Either Error Value
less [IntVal a ,IntVal b] = if a < b then Right TrueVal
                  else Right FalseVal
less [StringVal s1,StringVal s2] = if s1 < s2 then Right TrueVal
                                    else Right FalseVal
less _ = Left "Invalid comparison"


-- Not used to avoid error commented
-- -- gets the current environnment
-- getEnv :: SubsM Env
-- getEnv = SubsM (\(e0, _) -> return (e0,e0) )

-- setEnv :: Env -> SubsM Env
-- setEnv newEnv = SubsM (\(_, _) -> return (newEnv, newEnv))

-- -- -- should replace the variable environment with the result of applying f to it.
-- modifyEnv :: (Env -> Env) -> SubsM ()
-- modifyEnv f = SubsM (\(e0,_) ->  Right((),f e0))


putVar :: Ident -> Value -> SubsM ()
putVar name val = SubsM (\(e0,_) -> Right((), Map.insert name val e0))


-- should read the value of the variable i in the current environment
getVar :: Ident -> SubsM Value
getVar name =  SubsM (\(e0,_) -> case lookup name (Map.assocs e0) of
                                    Nothing -> Left ("unbound variable: "++name)
                                    Just v -> Right (v,e0))

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\(e0,pe0) ->  case lookup name $ Map.assocs pe0 of
                                           Nothing -> Left ("unbound function:" ++ name)
                                           Just f -> Right (f,e0))
checkVar :: Ident -> SubsM (Maybe Value)
checkVar name = SubsM (\(e0,_) -> case lookup name (Map.assocs e0) of
                                         Nothing -> Right (Nothing, e0)
                                         Just val -> Right (Just val, e0))

deleteVar :: Ident -> SubsM ()
deleteVar name = SubsM (\(e0,_) -> Right((), Map.delete name e0))

-- Expressions evaluation

runExpr :: Expr -> Either Error Value
runExpr expr = do
  (val, _) <-runSubsM (evalExpr expr) initialContext
  return val


-- data Expr = Number Int
--          | String String
--          | Array [Expr]
--          | Undefined
--          | TrueConst
--          | FalseConst
--          | Var Ident
--          | Compr ArrayCompr
--          | Call FunName [Expr]
--          | Assign Ident Expr
--          | Comma Expr Expr
--          deriving (Eq, Read, Show)
evalExpr :: Expr -> SubsM Value
evalExpr (Number n) = return (IntVal n)

evalExpr (String n) = return (StringVal n)

evalExpr Undefined = return UndefinedVal

evalExpr TrueConst = return TrueVal

evalExpr FalseConst = return FalseVal

evalExpr (Var name) = getVar name

evalExpr (Comma a b) = evalExpr a >> evalExpr b

evalExpr (Assign ident expr) = do
  a <- evalExpr expr
  putVar ident a
  return a

evalExpr (Array l) = do
  val <- mapM evalExpr l
  return (ArrayVal val)

evalExpr (Call fname l) = do
  funct <- getFunction fname
  v <- mapM evalExpr l
  case funct v of
    Left err -> fail err
    Right val -> return val

evalExpr (Compr (ACBody expr)) = evalExpr expr

evalExpr (Compr (ACIf e acompr)) = do
  val <- evalExpr e
  case val of
    TrueVal -> evalExpr (Compr acompr)
    FalseVal -> return (ArrayVal [])
    _ -> return UndefinedVal

evalExpr (Compr (ACFor ident expr arrayCompr)) = do
  currentVar <- checkVar ident
  evaluated <- evalExpr expr
  case evaluated of
    (StringVal a) -> do
      result <- evalForStringCompr ident (StringVal a) arrayCompr
      _ <- handleForEnd currentVar ident
      return (ArrayVal (flatten result))
    (ArrayVal a)-> do
      result <- evalForIntCompr ident (ArrayVal a) arrayCompr
      _ <- handleForEnd currentVar ident
      return (ArrayVal (flatten result))
    _ -> return (ArrayVal [])

handleForEnd :: Maybe Value -> Ident -> SubsM ()
handleForEnd Nothing ident = deleteVar ident
handleForEnd (Just value) ident = putVar ident value

evalForStringCompr :: Ident -> Value -> ArrayCompr -> SubsM [Value]
evalForStringCompr _ (StringVal []) _ = return [StringVal []]
evalForStringCompr ident (StringVal (h:t)) arrayCompr = do
  putVar ident (StringVal [h])
  val <- evalExpr (Compr arrayCompr)
  rest <- evalForStringCompr ident (StringVal t) arrayCompr
  return (val:rest)
evalForStringCompr _ _ _ = return [UndefinedVal]

evalForIntCompr :: Ident -> Value -> ArrayCompr -> SubsM [Value]
evalForIntCompr _ (ArrayVal []) _ = return [ArrayVal []]
evalForIntCompr ident (ArrayVal (IntVal h:t)) arrayCompr = do
  putVar ident (IntVal h)
  val <- evalExpr (Compr arrayCompr)
  rest <- evalForIntCompr ident (ArrayVal t) arrayCompr
  return (val:rest)
evalForIntCompr _ _ _ = return [UndefinedVal]

flatten :: [Value] -> [Value]
flatten [] = []
flatten (ArrayVal h:t) = flatten h ++ flatten t
flatten (h:t) =  h : flatten t


-- something :: SubsM Value
-- something = do putVar "x" (IntVal 5)
--                env <- getEnv
--                putVar "x" (IntVal 10)
--                val  <- getVar "x"
--                setEnv env
--                val2 <- getVar "x"
--                return val2
-- main :: IO ()
-- main = do let result = case runSubsM something initialContext of
--                                   Left errMsg -> errMsg
--                                   Right (a,e) -> (show a)
--           putStrLn (show result)


   -- Comma (Assign "x" (Number 0))
   --          (Comma (Compr (ACFor "y"
   --                               (Array [Number 1,Number 2,Number 3])
   --                               (ACBody (Assign "x" (Var "y")))))
   --                 (Var "x"))