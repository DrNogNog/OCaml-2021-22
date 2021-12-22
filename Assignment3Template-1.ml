(*
Honor code comes here:

First Name: Gordon
Last Name: Ng
BU ID: U82744816

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(* the type of a polymorphic tree *)
type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree

(*
write a map function for trees:

For example,
map_tree (fun x -> x+1) (Node (Leaf 1, Leaf 2)) =  (Node (Leaf 2, Leaf 3))
map_tree (fun _ -> 0)  (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
                       (Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Leaf 0    )))
*)
let rec map_tree (f: 'a -> 'b) (tree: 'a tree): 'b tree = 
  match tree with
  | Leaf i -> Leaf (f i) 
  | Node (left,right) -> Node(map_tree f left, map_tree f right)
(*
write a fold function for trees:

For example,
fold_tree ( * ) (fun x -> x) (Node (Leaf 3, Leaf 2)) = 6
fold_tree (+) (fun _ -> 1) (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) = 7
*)

let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  =
  match tree with
  |Leaf x -> leaf x
  |Node (l,r) -> node (fold_tree node leaf l) (fold_tree node leaf r)

(*
sum the contents of an int tree

For example,
sum_ints (Node (Leaf 1, Leaf 2)) = 3
*)
let rec sum_ints (tree: int tree): int  = 
  match tree with
  |Leaf x -> x
  |Node (l,r) -> (sum_ints l) + (sum_ints r)
(*
find the size of the tree

For example,
tree_size (Leaf 1) = 1
tree_size (Node (Leaf 1, Leaf 2)) = 3
*)
let rec tree_size (tree: 'a tree): int  = 
  let rec auxiliary tree acc =
    match tree with
    |Leaf x -> acc + 1 
    |Node (left,right) ->(auxiliary left acc) + (auxiliary right acc)+1
  in auxiliary tree 0
  
  
(*
find the height of the tree

For example,
tree_height (Leaf 2) = 1
tree_height (Node ((Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))), Leaf 2)) = 5
*)
let rec tree_height (tree: 'a tree): int  = 
  let rec auxiliary tree acc =
    match tree with
    |Leaf x -> acc + 1
    |Node (left,right) -> if ((auxiliary left acc) > (auxiliary right acc)) then (auxiliary left acc) + 1 else (auxiliary right acc) + 1 
  in auxiliary tree 0
  
 (* let rec auxiliary tree acc =
    |Node(left,right) -> if ((aux left acc) < (aux right acc)) then (aux right acc) + 1 else (aux left acc)+1
	|Leaf x -> acc + 1
  in aux tree 0
*)

(*
write a function that takes a predicate on trees and retuns true if any subtree satisfies that predicate

For example,
tree_contains (Node (Leaf 1, Leaf 2)) (fun x -> match x with Leaf 2 -> true | _ -> false) = true
tree_contains (Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))) (fun x -> tree_height x > 2) = true
*)

let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool  = 
  match tree with
  | Node (left,right) -> if (look_for (Node (left,right)))== true then true 
						else if (tree_contains left look_for) == true then true
						else if (tree_contains right look_for) == true then true 
						else false 
  | Leaf x -> (look_for (Leaf x))


(*
write a function that shows bool trees :

For example,
show_bool_tree (Leaf true) ="true"
show_bool_tree (Node (Leaf true, Leaf false)) = "(true^false)" 
show_bool_tree  (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
    "((true^(true^false))^((true^(true^false))^false))" 
*)

let rec show_bool_tree (tree: bool tree) : string  =
  let rec aux tree acc =
    match tree with
    |Node (left,right) -> "(" ^ aux left acc ^ "^" ^ aux right acc ^ ")"
    |Leaf y -> if (acc = "") then (if y == true then "true" else "false") else (if y == true then "true" else "false") ^ "^" ^ acc
  in aux tree ""

(* standard functions to convert between string and char list *)
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)

(*
write a function that reads bool trees :
for all (finite) t : bool trees.
read_bool_tree t = Some (show_bool_tree t)

For example,
read_bool_tree "true" = Some (Leaf true)
read_bool_tree "false" = Some (Leaf false)
read_bool_tree "tralse" = None
read_bool_tree "(true^false)" = Some (Node (Leaf true, Leaf false))
read_bool_tree "((true^(true^false))^((true^(true^false))^false))" =
Some
 (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false)))
*)

(* Hint 
write a helper function 
read_bool_prefix : (char list) -> ((bool * (char list)) option) 

such that
read_bool_prefix (explode "true???")       = Some (true, ['?'; '?'; '?'])
read_bool_prefix (explode "false123")      = Some (false, ['1'; '2'; '3'])
read_bool_prefix (explode "antythingales") = None
read_bool_prefix []                        = None

write a helper function 
read_bool_tree_prefix (char list) -> ((bool tree * (char list)) option) 

such that
read_bool_tree_prefix [] = None
read_bool_tree_prefix (explode "true???") = Some (Leaf true, ['?'; '?'; '?'])
read_bool_tree_prefix (explode "(true^false)124") = Some (Node (Leaf true, Leaf false), ['1'; '2'; '4'])
read_bool_tree_prefix (explode "(true^(true^false))aaa") = Some (Node (Leaf true, Node (Leaf true, Leaf false)), ['a'; 'a'; 'a'])
read_bool_tree_prefix (explode "(true^(true^fa se))aaa") = None
*)
let rec read_bool_prefix  (x :char list) :((bool * (char list)) option) =
  match x with
  | 't' :: 'r' :: 'u' :: 'e' :: rest -> (Some (true,(rest)))
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> (Some (false,(rest)))
  | _ -> None
    
let rec read_bool_tree_prefix (s :char list) : ((bool tree * (char list)) option) = 
  match (read_bool_prefix s, s) with 
  |(Some (r, tree), _) -> Some (Leaf r, tree) 
  |(r, '(' :: tree) -> 
      
      (match read_bool_tree_prefix tree with
       |None -> None 
       |Some (treett, '^' :: tree) -> 

           (match read_bool_tree_prefix tree with
            |Some(treet, ')' :: tree) -> Some (Node (treett, treet), tree) 
            |None -> None
            |Some (p,u) -> None 
            |Some _ -> None))
        (*could not get read_bool_tree_prefix (explode "(true^(true^fa se))aaa") = None *) 

let rec read_bool_tree (s: string) : ((bool tree) option) =
  match (read_bool_tree_prefix (explode s)) with
  |None -> None
  |Some (trees, list) -> Some trees

(*
write a fubction that checks that parenthisis are balnaced:
Parenthisis are balenced if there are no parenthises
Parenthisis are balenced if ( and )  enclose a balenced parenthises
Parenthisis are balenced if balenced parenthises are ajacent to a balenced parenthisis

For example,
matching_parens "" = true
  matching_parens "((((((((((()))))))))))" = true
  matching_parens "()()()()()()" = true
  matching_parens "(()())" = true
  matching_parens "())(()" = false
                             *)

(* Hint 
write mutually recursive functions 
matching_paren_prefix : (char list) -> ((char list) option)
matching_parens_prefix : (char list) -> ((char list) option)

the and keyword allows mutual recursion
let rec matching_paren_prefix (ls: char list) : ((char list) option) = failwith "unimplemented"
and matching_parens_prefix  (ls: char list) : ((char list) option) = failwith "unimplemented"

such that
matching_paren_prefix [] = None
matching_paren_prefix (explode "(???") = None
matching_paren_prefix (explode "()???") = Some ['?'; '?'; '?']
matching_paren_prefix (explode "(((())))123") = Some ['1'; '2'; '3']
matching_paren_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
matching_paren_prefix (explode "(()()())abc") = Some ['a'; 'b'; 'c']

matching_parens_prefix [] = Some []
matching_parens_prefix (explode "()()()") = Some []
matching_parens_prefix (explode "()())))") = Some [')'; ')'; ')']
matching_parens_prefix (explode ")aa") = Some [')'; 'a'; 'a']
*)

let rec matching_parens (tree: string) : bool  = 
  let rec auxiliary tree acc =
    if acc >= 0 then
      match tree with
      | [] -> acc
      | '(' :: rest ->auxiliary rest (acc + 1)
      | ')' :: rest ->auxiliary rest (acc - 1)
      | _ :: rest-> auxiliary rest acc
    else 1
  in if (auxiliary (explode tree) 0) == 0 then true else false



