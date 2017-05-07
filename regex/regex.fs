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
                    let follow_a_b = Set.union (follow c a) (follow c b) 
                    if last_a.Contains c then Set.union follow_a_b first_b else follow_a_b  

let next_step r state c  =
  let foldingFunction q = function 
    | (c1,_) as c' -> if c1 = c then Set.union (follow c' r) q else q
  Set.fold foldingFunction Set.empty<ichar> state  

type state = Set<ichar>
type trans = Map<state*char, state>
type autom = {
  start : state;
  trans : trans;
  regex : regexp
}


let rec update f x =
  let to_continue, new_x = f x 
  if to_continue then update f new_x else new_x

let update_characters (s:state) (all_characters:Set<char>) (r:regexp) =
  Set.fold (fun trans c -> let ns =(next_step r s c)
                           if Set.isEmpty ns then
                            trans
                           else
                            Map.add (s, c) (next_step r s c) trans)  Map.empty<state*char, state> all_characters


let rec allCharacter  = function
  | Epsilon -> Set.empty<char>
  | Star a -> allCharacter a
  | Character a -> Set.singleton (fst a)
  | Union (a,b) | Concat (a,b) -> Set.union (allCharacter a) (allCharacter b)

let updateAutom (trans:trans) r   = 
  let all_states =  Map.fold (fun a _ c -> Set.union a (Set.singleton c) ) Set.empty<state> trans
  let all_characters = allCharacter r
  let new_trans = Map.toSeq trans
                  |> Seq.map snd
                  |> Seq.map (fun state -> update_characters state all_characters r)
                  |> Seq.append (Seq.ofList [trans])
                  |> Seq.collect Map.toList
                  |> Map.ofSeq
  new_trans = trans, new_trans
                  

