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
           -- Add other variants as needed
           deriving (Show, Read)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used



data MachineState = MachineState Stack Exp VEnv Flag String -- add the definition

type Stack = [Frame]

data Frame = FIf Exp Exp
           | FApp
           | FLet
           | FFun

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
msStep (MachineState stack (Var x) env Evaluating s) = undefined
msStep (MachineState stack (App e1 e2) env Evaluating s) = MachineState ((FApp) : stack) e1 (E.add env (s2, (E e2 Nil))) Evaluating s
  where
    s2 = s ++ ".f"
msStep (MachineState stack (If e1 e2 e3) env Evaluating s) = MachineState ((FIf e2 e3) : stack) e1 env Evaluating s
msStep (MachineState stack (Let binds e) env Evaluating s) = MachineState ((FLet) : stack) e (addBinds env binds s) Evaluating s2
  where
    s2 = s
msStep (MachineState stack (Recfun bind) env Evaluating s) = undefined
msStep (MachineState stack exp env Returning s) = MachineState newStack newExp env newFlag s
  where
    (newStack, newExp, newFlag) = insertVal stack (expToVal env exp)


addBinds :: VEnv -> [Bind] -> String -> VEnv
addBinds env binds string = undefined

insertVal :: Stack -> Value -> (Stack, Exp, Flag)
insertVal (FApp : stack) (I i) = undefined
insertVal (FLet : stack) (I i) = undefined
insertVal (FFun : stack) (I i) = undefined
insertVal (FApp : stack) (B b) = undefined
insertVal (FLet : stack) (B b) = undefined
insertVal (FFun : stack) (B b) = undefined
insertVal ((FIf e1 e2) : stack) (B b) = case b of True -> (stack, e1, Evaluating)
                                                  False -> (stack, e2, Evaluating)
insertVal stack (B b) = undefined


expToVal :: VEnv -> Exp -> Value
expToVal env (Con s) = case s of "True" -> B True
                                 "False" -> B False
                                 _ -> error "invalid constant"
expToVal env (Var x) = case E.lookup env x of Just e -> e
                                              Nothing -> error "variable not in environment"
expToVal env (Num i) = I i
expToVal _ _ = error "cannot cast exp to val"




































