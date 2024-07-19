type id = string
type binop = Add | Sub | Mul | Div

type stmt = Assign of id * expr | Compound of stmt * stmt | Print of expr list

and expr =
  | Id of id
  | Num of int
  | Op of expr * binop * expr
  | Eseq of stmt * expr

let max_args = function Print _ -> 2 | _ -> 0
let ( << ) f g x = f (g x)

let interpret expr_ =
  let table = [] in
  let update_symbol table id value = (id, value) :: List.remove_assq id table in
  let interpret_op = function
    | Add -> fun a b -> a + b
    | Sub -> fun a b -> a - b
    | Mul -> fun a b -> a * b
    | Div -> fun a b -> a / b
  in
  let rec interpret' table = function
    | Assign (id_, expr_) ->
        update_symbol table id_ @@ interpret_expr table expr_
    | Compound (stmt1, stmt2) -> interpret' (interpret' table stmt1) stmt2
    | Print exprs ->
        let _ =
          List.iter
            (fun expr_ ->
              (print_endline << string_of_int) @@ interpret_expr table expr_)
            exprs
        in
        table
  and interpret_expr table = function
    | Id id_ -> List.assoc id_ table
    | Num n -> n
    | Op (expr_, op, expr2) ->
        let op' = interpret_op op in
        op' (interpret_expr table expr_) (interpret_expr table expr2)
    | Eseq (st, expr_) -> interpret_expr (interpret' table st) @@ expr_
  in
  interpret' table expr_
;;

print_endline "Hello, World!"
