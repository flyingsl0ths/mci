open Token

type t

val mk_lexer : string -> t
val scan_token : t -> token * t
