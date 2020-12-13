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
           | R Exp Value
           | H Value
           | F String Exp
           deriving (Show, Read)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined



data MachineState = MachineState Stack State VEnv 
                  deriving (Show)

data State = Evaluating Exp
           | Returning Value
           deriving (Show)

type Stack = [Frame]

data Frame = FIf Exp Exp
           | FApp Exp
           | FOp Op
           | FPOp Value Op
           | FOp3 Op
           | FVar String
           | FList Value Exp
           | FClos VEnv
           | FCons Value
           | FIn Exp String
           | FHead 
           | FTail Value
           deriving (Show)

-- do not change this definition
evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE e

-- do not change this definition
evalE :: Exp -> Value
evalE exp = {-trace (show exp) $-} loop (msInitialState exp) 100000000 -- maximum amount of steps the program can make, to prevent infinite loops in bugged recursion cases
  where 
    loop ms@(MachineState stack state env) i =  --(trace $ show state ++ " " ++ show env) $
             if (msInFinalState newMsState) || (i < 0) 
                then msGetValue newMsState
                else loop newMsState (i - 1)
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
msGetValue ms = error $ "looped too long"
  
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
                                                   Just (E e) -> MachineState stack (Evaluating e) env
                                                   Just (R e val) -> MachineState ((FIn e x) : stack) (Returning val) env
                                                   Nothing -> error $ (show x) ++ " variable not in env " ++ show env
evalExp stack (If e1 e2 e3) env = MachineState ((FIf e2 e3) : stack) (Evaluating e1) env
evalExp stack (Let binds e1) env = MachineState ((FClos env) : stack) (Evaluating e1) (addBinds env binds)
evalExp stack (App e1 e2) env = makeApp stack (App e1 e2) env
evalExp stack (Recfun (Bind id t [] exp)) env = MachineState stack (Evaluating exp) (E.add env (id, E exp))
evalExp stack (Letrec binds exp) env = MachineState stack (Evaluating exp) (addBinds env binds)
evalExp stack e env = error $ show e
insertVal :: Stack -> Value -> VEnv -> MachineState
insertVal ((FIf e1 e2) : stack) val env = case val of B True -> MachineState stack (Evaluating e1) env
                                                      _ -> MachineState stack (Evaluating e2) env
insertVal ((FVar x) : stack) val env = MachineState stack (Returning val) (E.add env (x, val))
insertVal ((FOp op) : stack) val env = handleOp op stack val env
insertVal ((FPOp val1 op) : stack) val2 env = MachineState stack (Returning (doOp op val1 val2)) env
insertVal ((FClos env) : stack) val _ = MachineState stack (Returning val) env
insertVal ((FIn e x) : stack) val env = MachineState ((FVar x) : stack) (Evaluating e) (E.add env (x, val))
insertVal ((FApp e) : stack) (O op val) env = MachineState ((FPOp val op) : stack) (Evaluating e) env
insertVal ((FApp e) : stack) (H val) env = MachineState ((FCons val) : stack) (Evaluating e) env
insertVal ((FApp e1) : stack) (E e2) env = MachineState stack (Evaluating (App e2 e1)) env
insertVal ((FApp e1) : stack) val env = MachineState stack (Returning val) env
insertVal ((FCons (I i)) : stack) val env = MachineState stack (Returning (Cons i val)) env
insertVal ((FCons (E e)) : stack) val env = MachineState ((FTail val) : stack) (Evaluating e) env
insertVal ((FTail val1) : stack) (I i) env = MachineState stack (Returning (Cons i val1)) env
insertVal (FHead : stack) val env = MachineState stack (Returning (H val)) env
insertVal (s:tack) val env = error (show s ++ " " ++ show val) 


handleOp :: Op -> Stack -> Value -> VEnv -> MachineState
handleOp Neg stack (I i) env = MachineState stack (Returning (I (-i))) env
handleOp Head stack val env = case val of Nil -> error "head on empty list"
                                          Cons x _ -> MachineState stack (Returning (I x)) env
                                          _ -> error "head on non list"
handleOp Tail stack val env = case val of Nil -> error "tail on empty list"
                                          Cons _ x -> MachineState stack (Returning x) env
                                          _ -> error "tail on non list"
handleOp Null stack val env = case val of Nil -> MachineState stack (Returning (B True)) env
                                          Cons _ _ -> MachineState stack (Returning (B False)) env
                                          _ -> error "null on non list"
handleOp op stack val env = MachineState stack (Returning (O op val)) env

addAtEndOfList :: Value -> Value -> Value
addAtEndOfList Nil (I i) = Cons i Nil
addAtEndOfList (Cons i val1) val2 = Cons i (addAtEndOfList val1 val2)


makeApp :: Stack -> Exp -> VEnv -> MachineState
makeApp s@((FClos oldEnv) :stack) (App (App e1 e2) e3) env = MachineState ((FApp e3) : s) (Evaluating (App e1 e2)) env
makeApp stack (App (App e1 e2) e3) env = MachineState ((FApp e3) : (FClos env) : stack) (Evaluating (App e1 e2)) env
makeApp stack (App (Prim op) e) env = MachineState ((FOp op) : stack) (Evaluating e) env
makeApp stack (App (Con "Cons") e) env = MachineState (FHead : stack) (Returning (E e)) env
makeApp stack (App (Var x) e) env = MachineState stack (Evaluating (App newExp e)) newEnv
  where
    (newExp, newEnv) = case E.lookup env x of Just (E exp) -> (exp, env)
                                              Just (F fid exp) -> (exp, E.add env (fid, E e))
                                              Nothing -> error $ "var " ++ x ++ " not in env " ++ (show env)
makeApp stack (App (Recfun (Bind fid ftype [] e1)) e2) env = MachineState stack (Evaluating (App e1 e2)) env
makeApp stack (App f@(Recfun (Bind fid (Arrow t1 (Arrow t2 t3)) (farg:fargs) e1)) e2) env = MachineState stack (Returning (E e1)) newEnv
  where
    newEnv = case E.lookup env fid of Nothing -> E.add (E.add env (farg, E e2)) (fid, E f)
                                      Just x -> case E.lookup env farg of Nothing -> E.add env (farg, E e2)
                                                                          Just z -> E.add env (farg, R e2 z)
makeApp stack (App (Recfun (Bind fid ftype (farg:fargs) e1)) e2) env = MachineState ((FClos env) : stack) (Evaluating e1) newEnv
  where
    newEnv = case E.lookup env fid of Nothing -> E.add (E.add env (fid, E (Recfun (Bind fid ftype (farg:fargs) e1)))) (farg, E e2)
                                      Just _ -> case E.lookup env farg of Nothing -> E.add env (farg, E e2)
                                                                          Just z -> E.add env (farg, R e2 z)
makeApp stack e env = error $ show stack ++ " " ++ show e 

addBinds :: VEnv -> [Bind] -> VEnv
addBinds env [] = env
addBinds env ((Bind bid btype [] e) : binds) = addBinds (E.add env (bid, E e)) binds
addBinds env ((Bind bid btype (barg:bargs) e) : binds) = addBinds (E.add env (bid, F barg e)) binds

doOp :: Op -> Value -> Value -> Value
doOp Add (I i1) (I i2) = I (i1 + i2)
doOp Sub (I i1) (I i2) = I (i1 - i2)
doOp Mul (I i1) (I i2) = I (i1 * i2)
doOp Quot (I i1) (I i2) = if i2 == 0 then error "divide by zero" else I (quot i1 i2)
doOp Rem (I i1) (I i2) = I (mod i1 i2)
doOp Gt (I i1) (I i2) = B (i1 > i2)
doOp Ge (I i1) (I i2) = B (i1 >= i2)
doOp Lt (I i1) (I i2) = B (i1 < i2)
doOp Le (I i1) (I i2) = B (i1 <= i2)
doOp Eq (I i1) (I i2) = B (i1 == i2)
doOp Ne (I i1) (I i2) = B (i1 /= i2)