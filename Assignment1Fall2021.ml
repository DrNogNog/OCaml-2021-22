(*
Honor code comes here:
​
First Name: Gordon
Last Name: Ng
BU ID: U82744816
​
I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(*
a print_list function you find useful for debugging.
*)
let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l
  in let _ = print_string "[" 
  in let _ = aux ls
  in print_string "]" 

(*
TODO: Write a tail recursive function equalLists testing the equality 
of two lists of integers.
For example,
equalLists [1;2;3;4] [1;2;3;4] = true
equalLists [1;2;3;4] [2;3;4;5] = false
equalLists [1;2;3;4] [1] = false
*)

let rec equalLists (x: int list) (y: int list): bool =
  match x, y with
  | [], [] -> true
  |  _, [] -> false
  | [], _ -> false
  | ahead::atail, bhead::btail ->
    if ahead = bhead then equalLists atail btail
    else false


let string_of_list (l: int list):string =
  let rec aux comb l =
    match l with
    | [] -> comb
    | s::[] -> comb ^ string_of_int s
    | head::tail -> (comb ^ string_of_int head ^ ";") ^ aux comb tail
  in "[" ^ (aux "" l) ^ "]"

(*

TODO:
The function list_of_list_of_int_as_string takes in as input a list of list of 
int and evaluates into its string representation. 

Here are some test cases on how list_of_list_of_int_as_string must work: 
list_of_list_of_int_as_string [[1];[2]] will evaluate into "[[1];[2]]"
list_of_list_of_int_as_string [[1;2;3]] will evaluate into "[[1;2;3]]"
list_of_list_of_int_as_string [[1];[2];[3];[4]] will evaluate into "[[1];[2];[3];[4]]"
list_of_list_of_int_as_string [[1]] will evaluate into "[[1]]"
list_of_list_of_int_as_string [] will evaluate into "[]"
*)
let rec list_of_list_of_int_as_string (l:int list list): string =
  let rec aux = fun l1 ->
    match l1 with
    | [] -> ""
    | list::[] -> string_of_list list
    | list::tailList ->
        string_of_list list ^ ";" ^ aux tailList
  in "[" ^ (aux l) ^ "]"

(*
TODO:
Write a function called `repeat` that given a number and a length:
returns a list containing that number, n times.
If the length is zero or negative it should return an empty list.
The solution should be tail recursive
For example,
  repeat 4 7 = [4; 4; 4; 4; 4; 4; 4]
  repeat 3 3 = [3; 3; 3]
  repeat 10 0 = []
  repeat 4 1000000 does not stack overflow
*)

let rec repeat (x:int) (n:int): int list =
  let rec aux var num ntimes =
    if ntimes=0 then var
    else aux (num::var) num (ntimes - 1)
  in aux [] x n


(* 
TODO:
Hint: use the following helper function 
let rec insert (i: int) (list: int list): int list = fail with "unimplemented"

that takes a a number, an already sorted ls and returns a new sorted list with that number unsorted
for example,

insert 5 [1;3;5;7] = [1;3;5;5;7]
*)
let rec insert (x: int) (ls: int list): int list =
    match ls with
    | [] -> [x]
    | head::tail -> 
    if head < x then head::insert x tail
    else x::ls
      

let rec sort (ls: int list): int list =
  match ls with
  | [] -> []
  | x :: tail -> insert x (sort tail)
