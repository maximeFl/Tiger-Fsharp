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
    | (s,i)::tail -> if  s = value then 
                       i 
                     else
                       lookup value tail
    | _ -> 0

let update_table (mytable : table) value_id value = 
  (value_id, value)::mytable

let print_stm (i:int , mytable: table) =
  printfn "%i" i
  mytable 

//let interpExp (exp_a: exp) (table_a: table) =
//  // exp -> tablea -> (int * table)
//  (5, [("a", 5)])  



let rec interpStm mytable = function
  | CompoundStm(stm_a, stm_b) -> interpStm (interpStm mytable stm_a) stm_b
  | AssignStm(id_a, exp_a) -> let mynumber, mytable = interpExp exp_a mytable
                              update_table mytable id_a mynumber
  | PrintStm stm_list -> List.fold (fun t e -> print_stm (interpExp e t ))  mytable stm_list

and interpExp (exp_a: exp) (table_a: table) =
  match exp_a with 
    | IdExp id_a -> lookup  id_a table_a, table_a
    | NumExp int_a -> int_a, table_a
    | OpExp(exp_1, op,  exp_2) -> let  int_exp_1, table_a = interpExp exp_1 table_a
                                  let  int_exp_2, table_a = interpExp exp_2 table_a
                                  eval int_exp_1 int_exp_2 op, table_a
    | EseqExp(stm_a, exp_a) -> interpExp exp_a (interpStm stm_a)   