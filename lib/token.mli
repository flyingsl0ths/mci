type pos = int

type token =
  | Type of pos * pos
  | Var of pos * pos
  | Function of pos * pos
  | Break of pos * pos
  | Of of pos * pos
  | End of pos * pos
  | In of pos * pos
  | Nil of pos * pos
  | Let of pos * pos
  | Do of pos * pos
  | To of pos * pos
  | For of pos * pos
  | While of pos * pos
  | Else of pos * pos
  | Then of pos * pos
  | If of pos * pos
  | Array of pos * pos
  | Assign of pos * pos
  | Or of pos * pos
  | And of pos * pos
  | Ge of pos * pos
  | Gt of pos * pos
  | Le of pos * pos
  | Lt of pos * pos
  | Neq of pos * pos
  | Eq of pos * pos
  | Divide of pos * pos
  | Times of pos * pos
  | Minus of pos * pos
  | Plus of pos * pos
  | Dot of pos * pos
  | Rbrace of pos * pos
  | Lbrace of pos * pos
  | Rbrack of pos * pos
  | Lbrack of pos * pos
  | Rparen of pos * pos
  | Lparen of pos * pos
  | Semicolon of pos * pos
  | Colon of pos * pos
  | Comma of pos * pos
  | String of string * pos * pos
  | Int of int * pos * pos
  | Id of string * pos * pos
  | Eof of pos * pos
