type ichar = char * int

type regexp =
  | Epsilon
  | Character of ichar
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp

let rec Rnull = function 
  | Epsilon |  Star _ -> true
  | Character _ -> false
  | Union (a,b) -> Rnull a || Rnull b
  | Concat (a,b) -> Rnull a && Rnull b

let rec first = function
  | Epsilon ->  Set.empty<ichar>
  | Star a -> first a
  | Character a -> Set.singleton a 
  | Union (a,b) -> Set.union (first a) (first b)
  | Concat (a,b) -> if Rnull a then 
                     Set.union (first a) (first b)
                    else
                      first a

let rec last = function
  | Epsilon ->  Set.empty<ichar>
  | Star a -> last a
  | Character a -> Set.singleton a
  | Union (a,b) -> Set.union (last a) (last b)
  | Concat (a,b) -> if Rnull b then 
                     Set.union (last a) (last b)
                    else
                      last b

let rec follow c = function
  | Epsilon | Character _ ->  Set.empty<ichar>
  | Star a ->  let last_a = last a 
               let first_a = first a
               if last_a.Contains c then first_a else Set.empty<ichar>
  | Union (a,b) -> Set.union (follow c a) (follow c b)
  | Concat (a,b) -> let last_a = last a 
                    let first_b = first b 
                    if last_a.Contains c then first_b else Set.empty<ichar>

let next_step r state c  =
  let folding_function q = function 
    | (c1,_) as c' -> if c1 = c then Set.union (follow c' r) q else q
  Set.fold folding_function Set.empty<ichar> state  



type state = Set<ichar>

type autom = {
  start : state;
  trans : state Cmap.t Smap.t
}
