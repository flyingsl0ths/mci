type id = string
type binop = Add | Sub | Mul | Div

type stmt = Assign of id * expr | Compound of stmt * stmt | Print of expr list

and expr =
  | Id of id
  | Num of int
  | Op of expr * binop * expr
  | Eseq of stmt * expr

let interpret expr_ =
  let table = [] in
  let print_table =
    List.iter (fun (id, value) -> Printf.printf "%s: %d\n" id value)
  in
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
    | Compound (stmt1, stmt2) ->
        let table' = interpret' table stmt1 in
        print_table table';
        let table'' = interpret' table' stmt2 in
        print_table table'';
        table''
    | _ ->
        print_table table;
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

let program =
  Compound (Assign ("a", Op (Num 5, Add, Num 3)), Print [ Id "a"; Num 5 ])
;;

interpret program
