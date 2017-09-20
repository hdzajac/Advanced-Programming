module SubsInterpreter
       (
         Value(..)
       , runExpr
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
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

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

-- newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}
instance Functor SubsM where
  -- (a->b) -> M a -> M b
  -- (a->b) -> ((Env, PEnv) -> Either Error (a,Env)) -> ((Env, PEnv) -> Either Error (b,Env))
  -- -fmap f mf =  SubsM (\(e,pe0)-> (case (runSubsM mf (e,pe0))  of
  -- -                                  Left errMsg -> Left errMsg
  -- -                                  Right (a,e) -> Right ((f a),e)))
  fmap = liftM


instance Applicative SubsM where
  pure = return
  (<*>) = ap

instance Monad SubsM where
  -- return a :: a->SubsM ((Env, PEnv) -> Either Error (a,Env) )
  -- define cases for either
  return x = SubsM (\(e,_) -> Right (x,e))
  -- f >>= m ::  SubsM(Env, PEnv) -> Either Error (a,Env)
  --              -> (a -> SubsM ((Env, PEnv) -> Either Error (b,Env) ))
  --              -> SubsM (Env, PEnv) -> Either Error (b,Env)
  f >>= m = SubsM (\(e0,ep0) -> case runSubsM f (e0,ep0) of
                                  Left errMsg -> Left errMsg
                                  Right (a,e) -> runSubsM (m a) (e,ep0))
  fail s = SubsM (\_-> Left s)

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

-- -----------------------------------------------------------------------------
-- The === operator compares its arguments for structural equality, without any coercions. ??????? what does this mean
-- It accepts operands of any type, but comparison of, e.g., a string and a number
-- will always yield false.
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
                                                      _ -> Left "Error"

equality [StringVal s1,StringVal s2] = if s1 == s2 then Right TrueVal
                              else Right FalseVal
equality [_] = Left "Too few arguments"
equality [_, _] = Left "Types do not match"
equality _ = Left "Too many arguments"

-- --------------------------------------------------------------------------
-- On the other hand, the two arguments to the < operator
-- must either both be integers or both be strings, where strings are compared using
-- the usual lexicographic order.
less::[Value] -> Either Error Value
less [IntVal a ,IntVal b] = if a < b then Right TrueVal
                  else Right FalseVal
less [StringVal s1,StringVal s2] = if s1 < s2 then Right TrueVal
                                    else Right FalseVal
less _ = Left "Invalid comparison"

-- let (Writer (y, v')) = f x in
-- Writer (y, v `mappend` v')
-- is this applied on
-- should replace the variable environment with the result of applying f to it.
-- modifyEnv :: (Env -> Env) -> SubsM ()
-- modifyEnv f = SubsM (\(e0,_) ->  Right((),(f e0)))

-- should set the value of the variable i to v in the current environment
putVar :: Ident -> Value -> SubsM ()
putVar name val = SubsM (\(e0,_) -> Right((), Map.insert name val e0))

-- helper function
-- should read the value of the variable i in the current environment
getVar :: Ident -> SubsM Value
getVar name =  SubsM (\(e0,_) -> case lookup name $ Map.assocs e0 of
                                    Nothing -> Left ("unbound variable:"++name)
                                    Just v -> Right (v,e0))


-- Primitive::[Value] -> Either Error Value
-- SubsM {runSubsM :: Context -> Either Error (a, Env)}
-- SubsM {runSubsM :: Context -> Either Error (Primitive, Env)}
-- SubsM {runSubsM :: Context -> Either Error (Primitive, Env)}
-- helper function
-- should look up the function implementing primitive i.

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\(e0,pe0) ->  case lookup name $ Map.assocs pe0 of
                                           Nothing -> Left ("unbound function:" ++ name)
                                           Just f -> Right (f,e0))
-- data Expr = Number Int
--          | String String
--          | Array [Expr]
--          | Undefined
--          | TrueConst
--          | FalseConst
--          | Var Ident
--          | --Compr ArrayCompr
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
  let a = evalExpr expr
  _ <- fmap (putVar ident) a
  a

evalExpr (Array l) = do
  val <- mapM evalExpr l
  return (ArrayVal val)

evalExpr (Call fname l) = do
  funct <- getFunction fname
  v <- mapM evalExpr l
  case funct v of
    Left err -> fail err
    Right val -> return val

-- -- f [Value] -> Either Error Value' with `Value
-- evalExpr (Call functionName l) = do f <- getFunction functionName
--                                     guard (length l == 0) >> return Undefined
--                                     expr <- l
--                                     vals <- fmap evalExpr expr
--                                     (resutl, _) <- fmap f vals
--                                     return result
--                                     -- correctExpr <- guard (Undefined) >> fail
--                                     -- valList21 <- [ v | Right (v,_) <- [(evalExpr expr) | expr <- l]]
--                                     -- (result, _) <- (fmap f valList)
--                                     -- return result


-- dummy implemetation not to throw errors
evalExpr (Compr _) = return (IntVal 1)

runExpr :: Expr -> Either Error Value
runExpr expr = do
  (val, _) <-runSubsM (evalExpr expr) initialContext
  return val

-- main = equality [(IntVal 2),(IntVal 1)]
-- main = runExpr (Call "===" [Array [Number 2, Number 2], Array [Number 2, Number 2]])