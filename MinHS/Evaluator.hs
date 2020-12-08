module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Debug.Trace

type VEnv = E.Env Value

data Flag = Evaluating | Returning

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | Func Exp
           -- Add other variants as needed
           deriving (Show, Read)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used



data MachineState = MachineState [Frame] Exp Flag VEnv -- add the definition

data Frame = FApp Block Block
           | FIf Block Block Block
           | FLet Block
           | FValue

data FBind = FBind Id Type [Id] Block

data Block = Block | BValue Value | BExp Exp


-- do not change this definition
evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE e

-- do not change this definition
evalE :: Exp -> Value
evalE exp = loop (msInitialState exp)
  where 
    loop ms = -- (trace "debug message") $  -- uncomment this line and pretty print the machine state/parts of it to
                                            -- observe the machine states
             if (msInFinalState newMsState)
                then msGetValue newMsState
                else loop newMsState
              where
                 newMsState = msStep ms

msInitialState :: Exp -> MachineState
msInitialState exp = MachineState [] exp Evaluating E.empty

-- checks whether machine is in final state
msInFinalState :: MachineState -> Bool
msInFinalState (MachineState [FValue] (Var s) Returning env) = True
msInFinalState (MachineState [FValue] (Con s) Returning env) = True
msInFinalState (MachineState [FValue] (Num x) Returning env) = True
msInFinalState _                                             = False


-- returns the final value, if machine in final state, Nothing otherwise
msGetValue :: MachineState -> Value
msGetValue ms = case msInFinalState ms of True -> undefined
                                          _    -> Nil
  
msStep :: MachineState -> MachineState
msStep (MachineState stack exp Returning env) = error "implement me!"
msStep (MachineState stack (Var s) Evaluating env) = undefined
msStep (MachineState stack (Prim op) Evaluating env) = undefined
msStep (MachineState stack (Con s) Evaluating env) = undefined
msStep (MachineState stack (Num x) Evaluating env) = undefined
msStep (MachineState stack (App e1 e2) Evaluating env) = (MachineState ((FApp Block Block):stack) e2 Evaluating env)
msStep (MachineState stack (If e1 e2 e3) Evaluating env) = undefined
msStep (MachineState stack (Let (b:bs) e) Evaluating env) = undefined
msStep (MachineState stack (Recfun b) Evaluating env) = undefined
msStep (MachineState stack (Letrec (b:bs) e) Evaluating env) = undefined

insertValue :: Frame -> Value -> Frame
insertValue (FApp Block block) = undefined
insertValue (FApp block Block) = undefined
insertValue (FIf Block block1 block2) = undefined
insertValue (FIf (BValue (B bool)) block1 block2) = undefined

insertValue (FLet Block) = undefined

getNextExp :: [Frame] -> Exp
getNextExp = undefined


















