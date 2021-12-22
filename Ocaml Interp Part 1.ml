(*gng8@bu.edu*)
(*U82744816*)


(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'
let is_upper_case c =
  'A' <= c && c <= 'Z'
let is_alpha c =
  is_lower_case c || is_upper_case c
let is_digit c =
  '0' <= c && c <= '9'
  
let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c
let is_blank c =
  String.contains " \012\n\r\t" c
  
let explode s =
  List.of_seq (String.to_seq s)
let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)
(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
    match ls with
    | x :: ls -> Some (x, ls)
    | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
    match ls with
    | x :: ls ->
        if f x then Some (x, ls)
        else None
    | _ -> None
let satisfy_function_avoiding_for_loop (f : string -> bool) : string parser =
  fun ls ->
    match ls with
    | x :: x2 :: ls ->
        if f (implode (x::[x2])) then Some ((implode (x::[x2])), ls)
        else None
    | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
    match p1 ls with
    | Some (_, ls) -> p2 ls
    | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
    match p1 ls with
    | Some (x, ls) ->
        (match p2 ls with
         | Some (_, ls) -> Some (x, ls)
         | None -> None)
    | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
    match p1 ls with
    | Some (x, ls)  -> Some (x, ls)
    | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> Some (f a, ls)
    | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
    match p ls with
    | Some (x, ls) ->
        (match many p ls with
         | Some (xs, ls) -> Some (x :: xs, ls)
         | None -> Some (x :: [], ls))
    | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
    match p ls with
    | Some (x, ls) ->
        (match many p ls with
         | Some (xs, ls) -> Some (x :: xs, ls)
         | None -> Some (x :: [], ls))
    | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
    match p () ls with
    | Some (x, ls) ->
        (match many' p ls with
         | Some (xs, ls) -> Some (x :: xs, ls)
         | None -> Some (x :: [], ls))
    | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
    match p () ls with
    | Some (x, ls) ->
        (match many' p ls with
         | Some (xs, ls) -> Some (x :: xs, ls)
         | None -> Some (x :: [], ls))
    | None -> None

let whitespace : unit parser =
  fun ls ->
    match ls with
    | c :: ls ->
        if String.contains " \012\n\r\t" c
        then Some ((), ls)
        else None
    | _ -> None

let ws : unit parser =
  (many whitespace) >| ()
let ws1 : unit parser =
  (many1 whitespace) >| ()
let digit : char parser =
  satisfy is_digit

let natural : int parser =
  fun ls ->
    match many1 digit ls with
    | Some (xs, ls) ->
        Some (int_of_string (implode xs), ls)
    | _ -> None
let literal (s : string) : unit parser =
  fun ls ->
    let cs = explode s in
    let rec loop cs ls =
      match cs, ls with
      | [], _ -> Some ((), ls)
      | c :: cs, x :: xs ->
          if x = c
          then loop cs xs
          else None
      | _ -> None
    in loop cs ls
let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()

(* end of parser combinators *)
(*-----------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------*)
(*----------------------------BREAKER-BREAKER----------------------------------*)
(*-----------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------*)

type const = NaturalNumber of int 
(*Natural Number needs to be checked*)
| NameVal of string | AUnit  
type command = Push of const | Add | Sub (*PEMDAS*)
| Mul | Div | Trace 
| IfThenElseThenEnd of (command list *command list)
type progq = Prog of command list

let const_natural_number_checker ch : bool = 
  if is_alpha ch || ch = '_' 
  then true 
  else false 
let const_aunit_checker str   :   bool = 
  match parse (keyword "()") str with
  |None -> false 
  |Some z ->  true


(*----PARSERS FOR COMMANDS----*)
let parser_for_constants :    const    parser = 
(*binds natural parser until you get a parser; function q and parser p turns into another parser*) 
  ((satisfy_function_avoiding_for_loop const_aunit_checker) >> pure AUnit) <|>
  (natural >>= fun q -> pure (NaturalNumber q)) <|> 
  (*pure is a return function, and it returns a value, used in conjunction with bind to return parser*) 
  (many1 (satisfy const_natural_number_checker ) >>= fun c -> pure (NameVal (implode c)))
  
let add_top_two_parser = keyword "Add" >>= fun q -> pure Add
let sub_top_two_parser = keyword "Sub" >>= fun q -> pure Sub
let mul_top_two_parser = keyword "Mul" >>= fun q -> pure Mul
let div_top_two_parser = keyword "Div" >>= fun q -> pure Div 

let  push_const_to_stack  = 
  keyword "Push" >>= fun q ->
  ws >>= fun _ -> parser_for_constants >>= fun q ->
  ws >>= fun _ -> pure (Push q) 

let  trace_top_value_to_string_parser  = 
  keyword "Trace" >>= fun q ->
  pure Trace

let rec recursive_combinator_function () =  
  push_const_to_stack <|> add_top_two_parser <|> sub_top_two_parser <|>
  mul_top_two_parser <|> div_top_two_parser <|> trace_top_value_to_string_parser <|>
  
  ifElseEndParser () and combination_function_q () = 
  
                       many' recursive_combinator_function
and  ifElseEndParser () = keyword "If" >>= fun q ->
  
  ws >>= fun _ -> combination_function_q () >>= fun qq ->
  
  ws >>= fun _ -> keyword "Else" >>= fun qqq -> 
  
  ws >>= fun _ -> combination_function_q () >>= fun qqqq ->
  
  ws >>= fun _ -> keyword "End" >>= fun qqqqq -> 
  pure (IfThenElseThenEnd(qq, qqqq))
  
let constant_function_string (aconstant: const) : string = 
  match aconstant with 
  | NaturalNumber q -> string_of_int q | NameVal value->value | AUnit->"()"

let interpreter_program_parser = 
  recursive_combinator_function () 
    
  >>= fun q -> many (ws
  
                     >>= fun _ -> recursive_combinator_function () 
                                                                
                     >>= fun q -> pure q) 
                    
  >>= fun progq -> pure (q::progq)

let constantl_function_stringl (constantlst: const list) : string list = 
  let rec aux tile someaccmulator = 
    match tile with 
    |[] -> someaccmulator 
    |h::t -> aux t ( ( (constant_function_string h)::someaccmulator ) )
  in aux constantlst []
  
let rec interpreter_expressions (command_list: command list) (stack: const list) (array_of_stack: const list):((const list *const list) option)= 
  match (command_list) with 
  |Add::tail -> 
      (match stack with 
       |top::bottom::resultant -> 
           (match top, bottom with 
            |NaturalNumber top, NaturalNumber bottom ->  interpreter_expressions tail (NaturalNumber (top + bottom)::resultant) array_of_stack
            |_ -> None)
       | _ -> None)
  |Sub::tail -> 
      (match stack with 
       |top::bottom::resultant -> 
           (match top, bottom with 
            |NaturalNumber top, NaturalNumber bottom ->  interpreter_expressions tail (NaturalNumber (bottom - top)::resultant) array_of_stack
            |_ -> None)
       | _ -> None)
  |Mul::tail -> 
      (match stack with 
       |top::bottom::resultant -> 
           (match top, bottom with 
            |NaturalNumber top, NaturalNumber bottom -> interpreter_expressions tail (NaturalNumber (top * bottom)::resultant) array_of_stack
            |_ -> None)
       | _ -> None)
  |Div::tail -> 
      (match stack with 
       |top::bottom::resultant -> 
           (match top, bottom with 
            |NaturalNumber top, NaturalNumber bottom -> if top>0 then interpreter_expressions tail (NaturalNumber (bottom / top)::resultant) array_of_stack else (None)
            |_ -> None)
       | _ -> None)
  |Push top::tail -> interpreter_expressions tail (top::stack ) array_of_stack 
  |Trace::tail -> 
      (match stack with 
       |h::t -> interpreter_expressions tail (AUnit::t) (h::array_of_stack)
       |[] -> None)
  | IfThenElseThenEnd (first, second):: tail ->  
      (match (match stack with 
           |NaturalNumber top::tail -> if top > 0 
               then interpreter_expressions first tail array_of_stack 
               else interpreter_expressions second stack array_of_stack 
           | _ -> None ) with 
       |Some (number, numtwo) -> 
           (match number with 
            |NaturalNumber h::resultant -> interpreter_expressions tail number numtwo
            |_ -> None)
       |_->None)
  |[] -> Some (stack,array_of_stack)

  
let interpreter (src : string) : (string * string list) =
  (match parse interpreter_program_parser src with 
   | Some ( a, []) -> 
       (match interpreter_expressions a [] [] with 
        | Some (h::t, array_of_stack) -> (constant_function_string h, List.rev(constantl_function_stringl array_of_stack))
        | None -> ("Error",[]))
		| Some (a, ls) -> ("Error",[])
        
	  (*Possible but Unused match cases*)
   (*| Some (_, _::_) -> ("Error", [])
   | None -> ("Error", [])*))


let exampleI = interpreter "Push 0
If
Push 123
Else
Push 12
If
Push 456
Else
Push 789
End
End
Push ()"
let exampleII = interpreter "Push 1
If
Push 123
Else
Push 456
End
Push ()"
let exampleIII  = interpreter "Push ()
If
Push 123
Else
Push 456
End
Push ()" 

(*val testI : string * string list = ("()", [])
val testII : string * string list = ("()", [])
val testIII : string * string list = ("Error", [])*)

(*TRACES*)
(*val trace_top_value_to_string_parser : command parser = <fun>
val recursive_combinator_function : unit -> command parser = <fun>
val combination_function_q : unit -> command list parser = <fun>
val ifElseEndParser : unit -> command parser = <fun>
val constant_function_string : const -> string = <fun>
val interpreter_program_parser : command list parser = <fun>
val constantl_function_stringl : const list -> string list = <fun>
val interpreter_expressions : 
command list -> const list -> const list -> (const list * const list) option = <fun>
val interpreter : string -> string * string list = <fun>*)