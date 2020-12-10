module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Debug.Trace

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | E Exp Value
           | F Exp
           | Unevaluated
           -- Add other variants as needed
           deriving (Show, Read)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used



data MachineState = MachineState Stack Exp VEnv Flag String -- add the definition
                    deriving (Show)

type Stack = [StackE]

data StackE = SFrame Frame | SEnv VEnv
              deriving (Show)

data Frame = FIf Exp Exp
           | FApp
           | FVar String
           deriving (Show)

data Flag = Evaluating | Returning
          deriving (Show)

-- do not change this definition
evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE e

-- do not change this definition
evalE :: Exp -> Value
evalE exp = loop (msInitialState exp)
  where 
    loop ms =  (trace (show ms)) $  -- uncomment this line and pretty print the machine state/parts of it to
                                            -- observe the machine states
             if (msInFinalState newMsState)
                then msGetValue newMsState
                else loop newMsState
              where
                 newMsState = msStep ms

msInitialState :: Exp -> MachineState
msInitialState exp = (trace (show exp)) $
                      MachineState [] exp E.empty Evaluating ""

-- checks whether machine is in final state
msInFinalState :: MachineState -> Bool
msInFinalState (MachineState [] (Num i) env Returning _) = True
msInFinalState (MachineState [] (Con s) env Returning _) = True
msInFinalState (MachineState [] (Var x) env Returning _) = error "implemt"
msInFinalState _ = False


-- returns the final value, if machine in final state, Nothing otherwise
msGetValue :: MachineState -> Value
msGetValue (MachineState [] (Num i) env Returning s) = I i
msGetValue (MachineState [] (Con "True") env Returning s) = B True
msGetValue (MachineState [] (Con "False") env Returning s) = B False
msGetValue (MachineState [] (Con "Nil") env Returning s) = Nil
msGetValue (MachineState [] (Var x) env Returning s) = error "implement me!"

{-
data Exp
    = Var Id
    | Prim Op
    | Con Id
    | Num Integer
    | App Exp Exp
    | If Exp Exp Exp
    | Let [Bind] Exp
    | Recfun Bind
    | Letrec [Bind] Exp
    deriving (Read,Show,Eq)
    -} 
 
msStep :: MachineState -> MachineState
msStep (MachineState stack (Con x) env Evaluating s) = MachineState stack (Con x) env Returning s
msStep (MachineState stack (Num i) env Evaluating s) = MachineState stack (Num i) env Returning s
msStep (MachineState stack (Var x) env Evaluating s) = case E.lookup env x of Just (I i) -> MachineState stack (Num i) env Returning s
                                                                              Just (B b) -> MachineState stack (Con (show b)) env Returning s
                                                                              Just (F exp) -> MachineState stack exp env Evaluating s
                                                                              Just Nil   -> MachineState stack (Con "Nil") env Returning s
                                                                              Just (E exp Unevaluated) -> MachineState ((SFrame (FVar x) ) : stack) exp env Evaluating s
                                                                              Just (E _ val) -> case val of (I i) -> MachineState stack (Num i) env Returning s
                                                                                                            (B b) -> MachineState stack (Con (show b)) env Returning s
msStep (MachineState stack (App e1 e2) env Evaluating s) = trace ((show e1) ++ " msStep") MachineState newStack newExp newEnv Evaluating s
  where
    (newStack, newExp, newEnv) = makeApp stack e1 e2 env
msStep (MachineState stack (If e1 e2 e3) env Evaluating s) = MachineState (SFrame (FIf e2 e3) : stack) e1 env Evaluating s
msStep (MachineState stack (Let binds e) env Evaluating s) = MachineState stack e (addBinds env binds s) Evaluating s
msStep (MachineState stack (Recfun (Bind id t ids exp)) env Evaluating s) = MachineState stack exp (E.add env (id, F exp ) ) Evaluating s
msStep (MachineState stack exp env Returning s) = MachineState newStack newExp newEnv newFlag s
  where
    (newStack, newExp, newFlag, newEnv) = insertVal stack (expToVal env exp) env


makeApp :: Stack -> Exp -> Exp -> VEnv -> (Stack, Exp, VEnv)
makeApp stack (Var f) varexp env = trace (show newExp) $ (stack, newExp, newEnv)
  where
    (newExp, newEnv) = case (E.lookup env f) of Just (F x@(Recfun (Bind fid ftype (farg:fargs) fexp))) -> (x, E.add env (farg, E varexp Unevaluated))
                                                Nothing -> error "function not in env"

addBinds :: VEnv -> [Bind] -> String -> VEnv
addBinds env [] string = env
addBinds env ((Bind id t ids exp):bs) string = trace ((show exp) ++ " addBinds") $  addBinds (E.add env (id, expToVal env exp)) bs string
--addBinds env ((Bind id (Arrow t1 t2) ids exp):bs) string = addBinds (E.add env (id, F exp)) bs string

insertVal :: Stack -> Value -> VEnv -> (Stack, Exp, Flag, VEnv)
insertVal ((SFrame (FApp)) : stack) (I i) env = undefined
insertVal ((SFrame (FApp)) : stack) (B b) env = undefined
insertVal ((SFrame (FIf e1 e2)) : stack) (B b) env = case b of True -> (stack, e1, Evaluating, env)
                                                               False -> (stack, e2, Evaluating, env)
insertVal ((SFrame (FVar x)) : stack) (B b) env = (stack, Con (show b), Returning, E.add env (x, B b))
insertVal ((SFrame (FVar x)) : stack) (I i) env = (stack, Num i, Returning, E.add env (x, I i))
insertVal ((SFrame (FVar x)) : stack) (F e) env = trace ((show e) ++ " insertVal" ) $ (stack, e, Evaluating, E.add env (x, F e))
insertVal stack (B b) env = undefined

getExpFromStack :: Stack -> Exp
getExpFromStack ((SFrame (FApp)):stack) = undefined
getExpFromStack ((SFrame (FIf e1 e2)) : stack) = undefined

expToVal :: VEnv -> Exp -> Value
expToVal env (Con s) = case s of "True" -> B True
                                 "False" -> B False
                                 "Nil" -> (trace "nilout") $  Nil
                                 _ -> error "invalid constant"
expToVal env (Var x) = case E.lookup env x of Just e -> e
                                              Nothing -> error "variable not in environment"
expToVal env (Num i) = I i
expToVal env (Recfun (Bind fname ftype fargs exp)) = trace ( (show exp) ++ " expToVal" ) $ F (Recfun (Bind fname ftype fargs exp))
expToVal _ exp = trace (show exp) $ error "cannot cast exp to val"




































