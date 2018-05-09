module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), (!?))
import Control.Monad.State
import Control.Monad.Except

data Expr
  = NumE     Int
  | BoolE    Bool
  | UnitE
  | IdE      String
  | AppE     Expr Expr
  | LambdaE  String Expr
  | BeginE   [Expr]
  | SetE     String Expr
  | IfE      Expr Expr Expr
  | LetE     [(String, Expr)] Expr

data Value
  = NumV      Int
  | BuiltinV  Builtin
  | UnitV
  | LambdaV   String Expr Env
  | BoolV     Bool
  | UndefinedV

data Builtin
  = PrintLnB
  | NegPB
  | ZeroPB
  | DecB

newtype Env = Env (Map String Location)

newtype Store = Store (Map Location Value)

newtype Location = Location Int
  deriving (Eq, Ord, Show)

instance Show Value where
  show (NumV n) = show n
  show (BuiltinV b) = show b
  show UnitV = "unit"
  show (LambdaV _ _ _) = "#<lambda>"
  show (BoolV True) = "true"
  show (BoolV False) = "false"

instance Show Builtin where
  show PrintLnB = "#<builtin println>"
  show NegPB = "#<builtin neg?>"
  show ZeroPB = "#<builtin zero?>"
  show DecB = "#<builtin dec>"

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
    BuiltinV bi -> appBuiltin bi ev
    LambdaV id expr env_0 -> do
      loc <- allocate
      setLocation loc ev
      let env_0_1 = bind id loc env_0
      evalE env_0_1 expr
    _ -> throwEval TypeError
  where
    appBuiltin PrintLnB arg = do
      liftIO . putStrLn . show $ arg
      return UnitV
    appBuiltin NegPB (NumV n) = do
      return . BoolV $ n < 0
    appBuiltin NegPB _ = throwEval TypeError
    appBuiltin ZeroPB (NumV n) = do
      return . BoolV $ n == 0
    appBuiltin ZeroPB _ = throwEval TypeError
    appBuiltin DecB (NumV n) = do
      return . NumV $ n - 1
    appBuiltin DecB _ = throwEval TypeError

evalE env (IdE id) = do
  loc <- lookupEnv id env
  v <- lookupStore loc
  case v of
    UndefinedV -> throwEval (UndefinedError id)
    _ -> return v

evalE env (BeginE exprs) = runE exprs
  where
    runE (e : x : xs) = evalE env e >> runE (x : xs)
    runE [e] = evalE env e
    run [] = return UnitV

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

evalE env (LetE bindings body) = do
  locs <- replicateM (length bindings) allocate
  let env' = foldr (uncurry bind) env (zip (map fst bindings) locs)
  values <- mapM (evalE env' . snd) bindings
  mapM_ (uncurry setLocation) (zip locs values)
  evalE env' body

mtEnv :: Env
mtEnv = Env $
  Map.fromList  [ ("println", Location 0)
                , ("zero?", Location 1)
                , ("dec", Location 2)
                ]

mtStore :: Store
mtStore = Store $
  Map.fromList [ (Location 0, BuiltinV PrintLnB)
               , (Location 1, BuiltinV ZeroPB)
               , (Location 2, BuiltinV DecB)
               ]

main :: IO ()
main = do
  let oddP =
        LambdaE "x"
        (IfE (AppE (IdE "zero?") (IdE "x"))
          (BoolE False)
          (AppE (IdE "even?") (AppE (IdE "dec") (IdE "x"))))
  let evenP =
        LambdaE "x"
        (IfE (AppE (IdE "zero?") (IdE "x"))
          (BoolE True)
          (AppE (IdE "odd?") (AppE (IdE "dec") (IdE "x"))))
  let prog =
        LetE
        [ ("i", (NumE 5))
        , ("odd?", oddP)
        , ("even?", evenP)
        , ("f", (LambdaE "x" (AppE (IdE "println") (IdE "i"))))
        ]
        (BeginE
          [ AppE (IdE "println") (AppE (IdE "odd?") (NumE 3))
          , AppE (IdE "f") UnitE
          , SetE "i" (NumE 6)
          , AppE (IdE "f") UnitE
          , LetE [("i", NumE 7)]
              (AppE (IdE "f") UnitE)
          , LetE
              [ ("fine", LambdaE "x" (AppE (IdE "fine") (IdE "x")))
              , ("notfine", AppE (IdE "dec") (IdE "notfine"))
              ]
              UnitE
          ])
  -- let i = 5
  --     odd? = x =>
  --       if zero?(x)
  --       then false
  --       else even?(dec(x))
  --     even? = x =>
  --       if zero?(x)
  --       then true
  --       else odd?(dec(x))
  --     f = x => println i
  -- {
  --   println(odd?(3))
  --   f()      ;; print 5
  --   i = 6
  --   f()      ;; print 6
  --   let i = 7
  --     f()      ;; print 6
  --   let fine = x => fine(x)
  --       notfine = dec(notfine)      ;; error
  --   ()
  -- }
  result <- eval mtEnv mtStore prog
  case result of
    Left err -> putStrLn ("error: " ++ show err)
    Right (v, _) -> putStrLn ("=> " ++ show v)
