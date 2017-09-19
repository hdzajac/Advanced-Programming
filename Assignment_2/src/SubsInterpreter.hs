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
type Env = Map Ident Value --variable aka symb table
type Primitive = [Value] -> Either Error Value  --preexisting func
type PEnv = Map FunName Primitive  --primitive env 
type Context = (Env, PEnv) 

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", undefined)
                       , ("<", undefined)
                       , ("+", add)
                       , ("*", multiply)
                       , ("-", minus)
                       , ("%", modulo)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  --(a->b) -> M a -> M b
  --(a->b) -> ((Env, PEnv) -> Either Error (a,Env)) -> ((Env, PEnv) -> Either Error (b,Env))
  ---fmap f mf =  SubsM (\(e,pe0)-> (case (runSubsM mf (e,pe0))  of
  ---                                  Left errMsg -> Left errMsg
  ---                                  Right (a,e) -> Right ((f a),e))) 

  fmap = liftM
  

instance Applicative SubsM where
  pure = return
  (<*>) = ap

instance Monad SubsM where
  -- return a :: a->SubsM ((Env, PEnv) -> Either Error (a,Env) )
  -- define cases for either
  return x = SubsM (\(e,pe) -> Right (x,e))
  -- f >>= m ::  SubsM(Env, PEnv) -> Either Error (a,Env) 
  --              -> (a -> SubsM ((Env, PEnv) -> Either Error (b,Env) ))
  --              -> SubsM (Env, PEnv) -> Either Error (b,Env)
  f >>= m = SubsM (\(e0,ep0) -> case (runSubsM f (e0,ep0)) of
                                        Left errMsg -> Left errMsg
                                        Right (a,e) -> runSubsM (m a) (e,ep0) )
  fail s = SubsM (\e-> Left s)
  
mkArray :: Primitive
mkArray [IntVal n] 
  | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

add :: Primitive
add [IntVal a, IntVal b] = return $ IntVal (a + b)
add [StringVal a, StringVal b] = return $ StringVal (a ++ b)
add [IntVal a, StringVal b] = return $ StringVal ((show a) ++ b)
add [StringVal a, IntVal b] = return $ StringVal (a ++ (Show b))
add _ = Left "\"+\" applied to incompatible types, try any combination of String and Int"

minus :: Primitive
minus [IntVal a, IntVal b] = return $ IntVal (a - b)
add _ = Left "\"-\" applied to incompatible types, try: Int - Int"


multiply :: Primitive
multiply [IntVal a, IntVal a] = return $ IntVal (a * b)
multiply _ = Left "\"*\" applied to incompatible types, try: Int * Int"

modulo :: Primitive
modulo [IntVal a, IntVal 0] = Left "\"% 0\" is undefined, try different value"
modulo [IntVal a, IntVal b] = return $ IntVal (a % b)
modulo _ = Left "\"*\" applied to incompatible types, try: Int % Int""



--let (Writer (y, v')) = f x in
--Writer (y, v `mappend` v')
-- is this applied on   
--should replace the variable environment with the result of applying f to it.
modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\(e0,pe0) ->  Right((),(f e0)))  

--should set the value of the variable i to v in the current environment
putVar :: Ident -> Value -> SubsM ()
putVar name val = SubsM (\(e0,pe0) ->  Right((),(Map.insert name val e0)))

--helper function
--should read the value of the variable i in the current environment
getVar :: Ident -> SubsM Value
getVar name =  SubsM (\(e0,pe0) ->  case (lookup name  $ Map.assocs  e0)  of
                                           Nothing -> Left ("unbound variable:"++name)
                                           Just v -> Right (v,e0))
										   
 
--Primitive::[Value] -> Either Error Value
--SubsM {runSubsM :: Context -> Either Error (a, Env)}
--SubsM {runSubsM :: Context -> Either Error (Primitive, Env)}
--SubsM {runSubsM :: Context -> Either Error (Primitive, Env)}
--helper function
--should look up the function implementing primitive i.
getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\(e0,pe0) ->  case (lookup name  $ Map.assocs  pe0)  of
                                           Nothing -> Left ("unbound function:"++name)
                                           Just v -> Right (v,e0))
--data Expr = Number Int
--          | String String
--          | ---Array [Expr]
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
evalExpr (String n) = return (StringVal n)
evalExpr Undefined = return (UndefinedVal)
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Var name) = do x <- getVar name 
                         return x
evalExpr (Call fname [l]) = do x <- getVar name 


runExpr :: Expr -> Either Error Value
runExpr expr = undefined
