open Token
open Errormsg

type t = {
  source : string;
  start : int;
  current : int;
  line : int;
  in_comment : bool;
}

let mk_lexer source =
  { source; start = 0; current = 0; line = 1; in_comment = false }

let advance ({ current; source; _ } as s) =
  ({ s with current = current + 1 }, source.[current])

let advance' ({ current; _ } as s) = { s with current = current + 1 }
let is_at_end { current; source; _ } = current >= String.length source

let peek ({ current; source; _ } as s) =
  if is_at_end s then None else Some source.[current]

let peek_next ({ current; source; _ } as s) =
  if is_at_end s then None else Some source.[current + 1]

let mk_token { start; current; source; _ } to_tk_type =
  let end' = current - start in
  let source = String.sub source start end' in
  to_tk_type (source, start, end')

let matches expected ({ current; source; _ } as s) =
  let no_match = is_at_end s || source.[current] <> expected in
  ((if no_match then s else advance' s), not @@ no_match)

let skip_whitespace s =
  let rec skip_whitespace' s' =
    match peek s' with
    | Some ' ' | Some '\r' | Some '\t' -> skip_whitespace' @@ advance' s'
    | Some '\n' -> skip_whitespace' @@ advance' { s' with line = s'.line + 1 }
    | Some '(' -> (
        match peek_next s' with
        | Some '*' ->
            let rec skip_comment s'' =
              if s''.in_comment then
                skip_comment @@ advance'
                @@
                match peek s'' with
                | Some c -> (
                    match c with
                    | '\n' -> { s'' with line = s''.line + 1 }
                    | '*' -> (
                        match peek_next s'' with
                        | Some ')' -> { s'' with in_comment = true }
                        | _ -> s'')
                    | _ -> s'')
                | _ -> s''
              else { s'' with in_comment = false }
            in
            skip_comment @@ advance' s'
        | _ -> s')
    | _ -> s
  in
  skip_whitespace' s

let is_digit = function '0' .. '9' -> true | _ -> false

let number lx =
  let rec has_digit lx' =
    match peek lx' with
    | Some c when is_digit c -> has_digit @@ advance' lx'
    | _ -> lx'
  in
  let has_decimal lx' =
    match (peek lx', peek_next lx') with
    | Some c, Some c' when c == '.' && is_digit c' -> has_digit @@ advance' lx'
    | _ -> lx'
  in
  let lx' = has_decimal @@ has_digit lx in
  mk_token lx' @@ fun (str, p1, p2) -> Int (int_of_string str, p1, p2)

let strng lx =
  let if_new_line lx' =
    match peek lx' with
    | Some c when c == '\n' -> { lx' with line = lx'.line + 1 }
    | _ -> lx'
  in
  let rec strng' lx' =
    match peek lx' with
    | Some c when c == '"' && (not @@ is_at_end lx') ->
        strng' @@ advance' @@ if_new_line lx'
    | _ -> lx'
  in
  mk_token (strng' lx) @@ fun (str, p1, p2) -> String (str, p1, p2)

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let identifier_type lxr =
  let at_most_n ?(n = 1) lxr' = lxr'.current - lxr'.start > n in

  let matches_last lxr' c f =
    match peek_next lxr' with
    | Some c' when c' == c -> f
    | _ -> fun (id, p1, p2) -> Id (id, p1, p2)
  in

  let check_keyword { current; start; source; _ } offset len rest to_token =
    let rest_matches =
      String.compare (String.sub source (start + offset) len) rest == 0
    in
    let matched = current - start == start + len && rest_matches in
    if matched then to_token else fun (id, p1, p2) -> Id (id, p1, p2)
  in
  match peek lxr with
  | Some c -> (
      match c with
      | 'a' -> check_keyword lxr 1 4 "rray" @@ fun (_, p1, p2) -> Array (p1, p2)
      | 'b' -> check_keyword lxr 1 4 "reak" @@ fun (_, p1, p2) -> Break (p1, p2)
      | 'd' when at_most_n lxr ->
          matches_last lxr 'o' (fun (_, p1, p2) -> Do (p1, p2))
      | 'e' when at_most_n lxr -> (
          match peek lxr with
          | Some c' -> (
              match c' with
              | 'l' ->
                  check_keyword lxr 2 2 "se" @@ fun (_, p1, p2) -> Else (p1, p2)
              | 'n' -> (
                  match peek_next lxr with
                  | Some c'' when c'' == 'd' -> fun (_, p1, p2) -> End (p1, p2)
                  | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
              | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
          | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
      | 'f' when at_most_n lxr -> (
          match peek lxr with
          | Some c' -> (
              match c' with
              | 'o' -> matches_last lxr 'r' (fun (_, p1, p2) -> For (p1, p2))
              | 'u' ->
                  check_keyword lxr 2 6 "nction" @@ fun (_, p1, p2) ->
                  Function (p1, p2)
              | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
          | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
      | 'i' when at_most_n lxr -> (
          match peek lxr with
          | Some c' -> (
              match c' with
              | 'f' -> fun (_, p1, p2) -> If (p1, p2)
              | 'n' -> fun (_, p1, p2) -> In (p1, p2)
              | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
          | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
      | 'l' -> check_keyword lxr 1 2 "et" @@ fun (_, p1, p2) -> Let (p1, p2)
      | 'o' -> matches_last lxr 'f' (fun (_, p1, p2) -> Of (p1, p2))
      | 't' -> (
          match peek lxr with
          | Some c' -> (
              match c' with
              | 'h' ->
                  check_keyword lxr 2 2 "en" @@ fun (_, p1, p2) -> Then (p1, p2)
              | 'y' ->
                  check_keyword lxr 2 2 "pe" @@ fun (_, p1, p2) -> Type (p1, p2)
              | 'o' -> fun (_, p1, p2) -> To (p1, p2)
              | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
          | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
      | 'v' -> check_keyword lxr 1 2 "ar" @@ fun (_, p1, p2) -> Var (p1, p2)
      | 'w' -> check_keyword lxr 1 4 "hile" @@ fun (_, p1, p2) -> While (p1, p2)
      | _ -> fun (id, p1, p2) -> Id (id, p1, p2))
  | _ -> fun (id, p1, p2) -> Id (id, p1, p2)

let identifier lx =
  let rec conforms lx' =
    match peek lx' with
    | Some c when is_alpha c || is_digit c -> conforms @@ advance' lx'
    | _ -> lx'
  in
  let lx' = conforms lx in
  mk_token lx' (identifier_type lx')

let scan_token lx =
  let lx' = skip_whitespace lx in
  let lx'' = { lx' with start = lx'.current } in
  if is_at_end lx'' then (mk_token lx'' (fun (_, p1, p2) -> Eof (p1, p2)), lx'')
  else
    let lx'', c = advance lx'' in
    let scan c lx =
      match c with
      | '0' .. '9' -> number lx
      | 'a' .. 'z' | 'A' .. 'Z' -> identifier lx
      | '"' -> strng lx
      | '(' -> mk_token lx @@ fun (_, p1, p2) -> Lparen (p1, p2)
      | ')' -> mk_token lx @@ fun (_, p1, p2) -> Rparen (p1, p2)
      | '{' -> mk_token lx @@ fun (_, p1, p2) -> Lbrace (p1, p2)
      | '}' -> mk_token lx @@ fun (_, p1, p2) -> Rbrace (p1, p2)
      | '[' -> mk_token lx @@ fun (_, p1, p2) -> Lbrack (p1, p2)
      | ']' -> mk_token lx @@ fun (_, p1, p2) -> Rbrack (p1, p2)
      | ';' -> mk_token lx @@ fun (_, p1, p2) -> Semicolon (p1, p2)
      | ',' -> mk_token lx @@ fun (_, p1, p2) -> Comma (p1, p2)
      | '.' -> mk_token lx @@ fun (_, p1, p2) -> Dot (p1, p2)
      | '-' -> mk_token lx @@ fun (_, p1, p2) -> Minus (p1, p2)
      | '+' -> mk_token lx @@ fun (_, p1, p2) -> Plus (p1, p2)
      | '/' -> mk_token lx @@ fun (_, p1, p2) -> Divide (p1, p2)
      | '*' -> mk_token lx @@ fun (_, p1, p2) -> Times (p1, p2)
      | '&' -> mk_token lx @@ fun (_, p1, p2) -> And (p1, p2)
      | '|' -> mk_token lx @@ fun (_, p1, p2) -> Or (p1, p2)
      | '=' -> mk_token lx @@ fun (_, p1, p2) -> Eq (p1, p2)
      | ':' ->
          let lx'', matched = matches '=' lx in
          if matched then mk_token lx'' @@ fun (_, p1, p2) -> Assign (p1, p2)
          else mk_token lx @@ fun (_, p1, p2) -> Colon (p1, p2)
      | '<' ->
          let lx'', matched = matches '>' lx in
          if matched then mk_token lx'' @@ fun (_, p1, p2) -> Neq (p1, p2)
          else
            let lx''', matched' = matches '=' lx in
            if matched' then mk_token lx''' @@ fun (_, p1, p2) -> Le (p1, p2)
            else mk_token lx''' @@ fun (_, p1, p2) -> Lt (p1, p2)
      | '>' ->
          let lx'', matched = matches '=' lx in
          if matched then mk_token lx'' @@ fun (_, p1, p2) -> Ge (p1, p2)
          else mk_token lx @@ fun (_, p1, p2) -> Gt (p1, p2)
      | _ ->
          Errormsg.error lx.start "Illegal character";
          raise Errormsg.Error
    in
    (scan c lx'', lx'')
