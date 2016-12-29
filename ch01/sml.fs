type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list
and exp = IdExp of id
        | NumExp of int
        | OpExp of exp * binop * exp
        | EseqExp of stm * exp

type table = (string * int ) list


let eval x y = function
  | Plus -> x + y
  | Minus -> x - y
  | Div -> x / y
  | Times -> x * y



  let rec lookup value (mytable : table) =
    match mytable with 
      | [] -> None
      | (s,i)::tail -> if  s = value then 
                        Some i 
                        else
                        lookup value tail

let update_table (mytable : table) value_id value = 
  (value_id, value)::mytable

