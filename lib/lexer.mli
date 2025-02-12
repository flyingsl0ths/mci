type t

val mk_lexer : string -> t
val lex : t -> Token.token * t
