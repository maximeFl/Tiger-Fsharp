type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list
and exp = IdExp of id
        | NumExp of int
        | OpExp of exp * binop * exp
        | EseqExp of stm * exp



let eval x y = function
  | Plus -> x + y
  | Minus -> x - y
  | Div -> x / y
  | Times -> x * y


