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

type Stack = [StackE]

data StackE = SFrame Frame | SEnv VEnv

data Frame = FIf Exp Exp
           | FApp String
           | FFun
           | FVar String

data Flag = Evaluating | Returning

-- do not change this definition
evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE e

-- do not change this definition
evalE :: Exp -> Value
evalE exp = loop (msInitialState exp)
  where 
    loop ms@(MachineState _ exp _ _ _) =  (trace (show exp)) $  -- uncomment this line and pretty print the machine state/parts of it to
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
                                                                              Just (F exp) -> MachineState stack exp env Returning s
                                                                              Just Nil   -> MachineState stack (Con "Nil") env Returning s
                                                                              Just (E exp Unevaluated) -> MachineState ((SFrame (FVar x) ) : stack) exp env Evaluating s
                                                                              Just (E _ val) -> case val of (I i) -> MachineState stack (Num i) env Returning s
                                                                                                            (B b) -> MachineState stack (Con (show b)) env Returning s
msStep (MachineState stack (App e1 e2) env Evaluating s) = MachineState (SFrame (FApp s2) : stack) e1 (E.add env (s2, (E e2 Unevaluated))) Evaluating s
  where
    s2 = s ++ ".f"
msStep (MachineState stack (If e1 e2 e3) env Evaluating s) = MachineState (SFrame (FIf e2 e3) : stack) e1 env Evaluating s
msStep (MachineState stack (Let binds e) env Evaluating s) = MachineState stack e (addBinds env binds s) Evaluating s
msStep (MachineState stack (Recfun (Bind id t ids exp)) env Evaluating s) = MachineState ((SFrame FFun) :stack) exp (E.add env (id, F exp ) ) Evaluating s
msStep (MachineState stack exp env Returning s) = MachineState newStack newExp newEnv newFlag s
  where
    (newStack, newExp, newFlag, newEnv) = insertVal stack (expToVal env exp) env

{-
data Bind = Bind Id Type [Id] Exp
  deriving (Read,Show,Eq)-}

addBinds :: VEnv -> [Bind] -> String -> VEnv
addBinds env [] string = env
addBinds env ((Bind id t ids exp):bs) string = addBinds (E.add env (id, expToVal env exp)) bs string
--addBinds env ((Bind id (Arrow t1 t2) ids exp):bs) string = addBinds (E.add env (id, F exp)) bs string

insertVal :: Stack -> Value -> VEnv -> (Stack, Exp, Flag, VEnv)
insertVal ((SFrame (FApp s)) : stack) (I i) env = undefined
insertVal ((SFrame (FApp s)) : stack) (B b) env = undefined
insertVal ((SFrame FFun) : stack) (I i) env = undefined
insertVal ((SFrame FFun) : stack) (B b) env = undefined
insertVal ((SFrame (FIf e1 e2)) : stack) (B b) env = case b of True -> (stack, e1, Evaluating, env)
                                                               False -> (stack, e2, Evaluating, env)
insertVal ((SFrame (FVar x)) : stack) (B b) env = (stack, Con (show b), Returning, E.add env (x, expToVal env (Con (show b))))
insertVal ((SFrame (FVar x)) : stack) (I i) env = (stack, Num i, Returning, E.add env (x, expToVal env (Num i)))
insertVal stack (B b) env = undefined

getExpFromStack :: Stack -> Exp
getExpFromStack ((SFrame (FApp s)):stack) = undefined
getExpFromStack ((SFrame (FIf e1 e2)) : stack) = undefined
getExpFromStack ((SFrame FFun) : stack) = undefined 

expToVal :: VEnv -> Exp -> Value
expToVal env (Con s) = case s of "True" -> B True
                                 "False" -> B False
                                 "Nil" -> (trace "nilout") $  Nil
                                 _ -> error "invalid constant"
expToVal env (Var x) = case E.lookup env x of Just e -> e
                                              Nothing -> error "variable not in environment"
expToVal env (Num i) = I i
expToVal _ _ = error "cannot cast exp to val"




































