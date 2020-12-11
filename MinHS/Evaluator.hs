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
           | E Exp
           | O Op Value
           -- Add other variants as needed
           deriving (Show, Read)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used



data MachineState = MachineState Stack State VEnv  -- add the definition
                  deriving (Show)

data State = Evaluating Exp
           | Returning Value
           deriving (Show)

type Stack = [Frame]

data Frame = FIf Exp Exp
           | FApp Exp
           | FOp1 Exp Op
           | FOp2 Value Op
           | FOp3 Op
           | FVar String
           | FList Value Exp
           | FClos VEnv
           deriving (Show)

-- do not change this definition
evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE e

-- do not change this definition
evalE :: Exp -> Value
evalE exp = trace (show exp) $ loop (msInitialState exp)
  where 
    loop ms =  (trace (show ms)) $  -- uncomment this line and pretty print the machine state/parts of it to
                                            -- observe the machine states
             if (msInFinalState newMsState)
                then msGetValue newMsState
                else loop newMsState
              where
                 newMsState = msStep ms

msInitialState :: Exp -> MachineState
msInitialState exp = MachineState [] (Evaluating exp) E.empty

-- checks whether machine is in final state
msInFinalState :: MachineState -> Bool
msInFinalState (MachineState [] (Returning _) e) = True
msInFinalState _ = False


-- returns the final value, if machine in final state, Nothing otherwise
msGetValue :: MachineState -> Value
msGetValue (MachineState [] (Returning val) env) = val
  
msStep :: MachineState -> MachineState
msStep (MachineState stack (Evaluating exp) env) = evalExp stack exp env
msStep (MachineState stack (Returning val) env) = insertVal stack val env

evalExp :: Stack -> Exp -> VEnv -> MachineState
evalExp stack (Con "True") env = MachineState stack (Returning (B True)) env
evalExp stack (Con "False") env = MachineState stack (Returning (B False)) env
evalExp stack (Con "Nil") env = MachineState stack (Returning Nil) env
evalExp stack (Num i) env = MachineState stack (Returning (I i)) env
evalExp stack (Var x) env = case E.lookup env x of Just (B b) -> MachineState stack (Returning (B b)) env
                                                   Just (I i) -> MachineState stack (Returning (I i)) env
                                                   Just (Cons i val) -> MachineState stack (Returning (Cons i val)) env
                                                   Just Nil -> MachineState stack (Returning Nil) env
                                                   Just (E e) -> MachineState ((FVar x) : stack) (Evaluating e) env
                                                   Nothing -> error "variable not in env"
evalExp stack (If e1 e2 e3) env = MachineState ((FIf e2 e3) : stack) (Evaluating e1) env
evalExp stack (Let binds e1) env = MachineState ((FClos env) : stack) (Evaluating e1) (addBinds env binds)
evalExp stack (App e1 e2) env = makeApp stack (App e1 e2) env
evalExp stack (Recfun (Bind id t ids exp)) env = MachineState stack (Evaluating exp) (E.add env (id, E exp))
--msStep (MachineState stack (Recfun (Bind id t ids exp)) env Evaluating s) = MachineState stack exp (E.add env (id, F exp ) ) Evaluating s

insertVal :: Stack -> Value -> VEnv -> MachineState
insertVal ((FIf e1 e2) : stack) val env = case val of B True -> MachineState stack (Evaluating e1) env
                                                      _ -> MachineState stack (Evaluating e2) env
insertVal ((FVar x) : stack) val env = MachineState stack (Returning val) (E.add env (x, val))
insertVal ((FOp1 e op) : stack) val env = MachineState ((FOp2 val op) : stack) (Evaluating e) env
insertVal ((FOp2 val1 op) : stack) val2 env = MachineState stack (Returning (doOp op val1 val2)) env
insertVal ((FOp3 Neg) : stack) (I i) env = MachineState stack (Returning (I (-i))) env
insertVal ((FClos env) : stack) val _ = MachineState stack (Returning val) env




makeApp :: Stack -> Exp -> VEnv -> MachineState
makeApp stack (App (App (Prim op) e1) e2) env = MachineState ((FOp1 e1 op) : stack) (Evaluating e2) env
makeApp stack (App (Prim Neg) e) env = MachineState ((FOp3 Neg) : stack) (Evaluating e) env
makeApp stack (App e1 e2) env = MachineState ((FApp e2) : stack) (Evaluating e1) env
makeApp stack (App (Var x) e) env = MachineState ((FClos env) : stack) (Evaluating func) newEnv
  where
    (func, newEnv) = case E.lookup env x of Just (E x@(Recfun (Bind fid ftype (farg:fargs) fexp))) -> (x, E.add env (farg, E e))
                                            Nothing -> error "function not in env "
    


addBinds :: VEnv -> [Bind] -> VEnv
addBinds env [] = env
addBinds env ((Bind bid btype bargs e) : binds) = addBinds (E.add env (bid, E e)) binds
addBinds env ((Bind bid btype bargs e) : binds) = addBinds (E.add env (bid, E e)) binds

doOp :: Op -> Value -> Value -> Value
doOp Add (I i1) (I i2) = I (i1 + i2)
doOp Sub (I i1) (I i2) = I (i1 - i2)
doOp Mul (I i1) (I i2) = I (i1 * i2)
doOp Quot (I i1) (I i2) = I (quot i1 i2)
doOp Rem (I i1) (I i2) = I (mod i1 i2)
doOp Gt (I i1) (I i2) = B (i1 > i2)
doOp Ge (I i1) (I i2) = B (i1 >= i2)
doOp Lt (I i1) (I i2) = B (i1 < i2)
doOp Le (I i1) (I i2) = B (i1 <= i2)
doOp Eq (I i1) (I i2) = B (i1 == i2)
doOp Ne (I i1) (I i2) = B (i1 /= i2)


