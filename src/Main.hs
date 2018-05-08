module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), (!?))
import Control.Monad.State
import Control.Monad.Except

data Stat
  = ExprS  Expr
  | DefS   String Expr

data Expr
  = NumE     Int
  | BoolE    Bool
  | UnitE
  | IdE      String
  | AppE     Expr Expr
  | LambdaE  String Expr
  | BeginE   [Stat]
  | SetE     String Expr
  | IfE      Expr Expr Expr
  | LetE     String Expr Expr

data Value
  = NumV      Int
  | BuiltinV  Builtin
  | UnitV
  | LambdaV   String Expr Env
  | BoolV     Bool
  | UndefinedV

data Builtin
  = PrintLnB

newtype Env = Env (Map String Location)

newtype Store = Store (Map Location Value)

newtype Location = Location Int
  deriving (Eq, Ord, Show)

instance Show Value where
  show (NumV n) = show n
  show (BuiltinV b) = show b
  show UnitV = "unit"
  show (LambdaV _ _ _) = "#<lambda>"
  show (BoolV b) = show b

instance Show Builtin where
  show PrintLnB = "#<builtin println>"

type Eval  = StateT Store (ExceptT EvalError IO)
data EvalError = TypeError | UndefinedError String | DanglingRefError Location
  deriving Show

throwEval :: EvalError -> Eval a
throwEval = lift . throwError

lookupEnv :: String -> Env -> Eval Location
lookupEnv k (Env e) = do
  case e !? k of
    Just loc -> return loc
    Nothing -> throwEval (UndefinedError k)

lookupStore :: Location -> Eval Value
lookupStore k = do
  Store store <- get
  case store !? k of
    Just value -> return value
    Nothing -> throwEval (DanglingRefError k)

allocate :: Eval Location
allocate = do
  Store store <- get
  let loc = Location (Map.size store)
  put (Store (Map.insert loc UndefinedV store))
  return loc

bind :: String -> Location -> Env -> Env
bind id loc (Env e) = Env (Map.insert id loc e)

setLocation :: Location -> Value -> Eval ()
setLocation loc v = do
  Store store <- get
  let store' = Map.alter f loc store
      f (Just _) = Just v
      f Nothing = error ("setLocation: unknown location " ++ loc2str loc)
      loc2str (Location loc) = show loc
  put (Store store')

eval :: Env -> Store -> Expr -> IO (Either EvalError (Value, Store))
eval env store expr = runExceptT (runStateT (evalE env expr) store)

evalE :: Env -> Expr -> Eval Value

evalE env (NumE n) = return (NumV n)

evalE env (BoolE b) = return (BoolV b)

evalE env UnitE = return UnitV

evalE env (AppE f e) = do
  fv <- evalE env f
  ev <- evalE env e
  case fv of
    BuiltinV PrintLnB -> do
      liftIO . putStrLn . show $ ev
      return UnitV
    LambdaV id expr env_0 -> do
      loc <- allocate
      setLocation loc ev
      let env_0_1 = bind id loc env_0
      evalE env_0_1 expr
    _ -> throwEval TypeError

evalE env (IdE id) = do
  loc <- lookupEnv id env
  v <- lookupStore loc
  case v of
    UndefinedV -> throwEval (UndefinedError id)
    _ -> return v

evalE env (BeginE exprs) = runE env UnitV exprs
  where
    runE :: Env -> Value -> [Stat] -> Eval Value
    runE env retval (s : ss) =
      case s of
        ExprS e -> do
          v <- evalE env e
          runE env v ss
        DefS id e -> do
          loc <- allocate
          let env' = bind id loc env
          v <- evalE env' e
          setLocation loc v
          runE env' UnitV ss
    runE env retval [] = return retval

evalE env (LambdaE id expr) = return (LambdaV id expr env)

evalE env (SetE id expr) = do
        v <- evalE env expr
        loc <- lookupEnv id env
        setLocation loc v
        return UnitV

evalE env (IfE cond then_ else_) = do
        cond' <- evalE env cond
        case cond' of
          BoolV True -> evalE env then_
          BoolV False -> evalE env else_
          _ -> throwEval TypeError

evalE env (LetE name value body) = do
  loc <- allocate
  let env' = bind name loc env
  value' <- evalE env' value
  setLocation loc value'
  evalE env' body

mtEnv :: Env
mtEnv = Env $
  Map.fromList  [ ("println", Location 0)
                ]

mtStore :: Store
mtStore = Store
  (Map.fromList [ (Location 0, BuiltinV PrintLnB)
                ])

main :: IO ()
main = do
  let prog = LetE "v" (NumE 5)
        (BeginE [ DefS "i" (IdE "v")
                , DefS "f" (LambdaE "x" (AppE (IdE "println") (IdE "i")))
                , ExprS (AppE (IdE "f") UnitE)
                , ExprS (SetE "i" (NumE 6))
                , ExprS (AppE (IdE "f") UnitE)
                , DefS "i" (NumE 7)
                , ExprS (AppE (IdE "f") UnitE)
                , ExprS (IfE (BoolE True)
                             (AppE (IdE "println") (NumE 1))
                             (AppE (IdE "println") (NumE 0)))
                , DefS "fine" (LambdaE "x" (AppE (IdE "fine") (IdE "x")))
                , DefS "notfine" (IdE "notfine")
                ])
  -- let v = 5 {
  --   var i = v
  --   var f = x => println i
  --   f()      ;; print 5
  --   i = 6
  --   f()      ;; print 6
  --   var i = 7
  --   f()      ;; print 6
  --   if true
  --   then print 1
  --   else print 0 ;; print 1
  --   var fine = x => fine(x)
  --   var notfine = notfine      ;; error
  -- }
  result <- eval mtEnv mtStore (BeginE [ExprS prog])
  case result of
    Left err -> putStrLn ("error: " ++ show err)
    Right (v, _) -> putStrLn ("=> " ++ show v)
