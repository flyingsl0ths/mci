open Token

type t = {
  source : string;
  start : int;
  current : int;
  line : int;
  gen_source_sub : bool;
}

let mk_lexer source =
  { source; start = 0; current = 0; line = 1; gen_source_sub = false }

let advance ({ current; _ } as s) =
  ({ s with current = current + 1 }, s.source.[current])

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
  let matches = is_at_end s || source.[current] <> expected in
  ((if matches then advance' s else s), matches)

let skip_whitespace s =
  let rec skip_whitespace' s' =
    match peek s' with
    | Some ' ' | Some '\r' | Some '\t' -> skip_whitespace' @@ advance' s'
    | Some '\n' ->
        skip_whitespace'
          { s' with line = s'.line + 1; current = s'.current + 1 }
    | Some '/' -> (
        match peek_next s' with
        | Some '/' ->
            let rec skip_comment s'' =
              match peek s'' with
              | Some '\n' | None -> s''
              | _ -> skip_comment @@ advance' s''
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
  has_decimal @@ has_digit lx

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
  strng' lx

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
    let matches = current - start == start + len && rest_matches in
    if matches then to_token else fun (id, p1, p2) -> Id (id, p1, p2)
  in
  match peek lxr with
  | Some c -> (
      match c with
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
