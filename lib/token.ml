(*
    Id => [a-zA-Z][a-zA-Z0-9_]*
    Comments => /\*.*\*/
    Integer => [0-9]*(\.)?[0-9]+
    String => '"' (Character | EscapeCharacter)* '"'
    Character => Id
    EscapeCharacter => '\' (n | t |  | ASCICode | '"' | \)
    ASCICode => [0-9][0-9][0-9]

    // No punctuation is needed to separate or terminate individual declarations.
    Declaration => {}
    Declaration => TypeDeclaration | VarDeclaration | FunctionDeclaration

    TypeDeclaration => "type" Id "=" Type
    Type => Type-Id | "{" TypeFields "}" | "array of" Type-Id

    TypeFields => ε | Id : Type-Id {, Id : Type-Id } 

    VarDeclaration => "var" Id ":=" Exp | "var" Id: Type-Id ":=" Exp

    FunctionDeclaration => 
        "function" Id "(" TypeFields ")" "=" Exp
      | "function" Id "(" TypeFields ")" : Type-Id "=" Exp

      Note:
        The scope of the parameters lasts until the end of the function, after Exp.

        The scope of a variable or parameter includes any scope declared up until the
        variable or parameter

    Note:
      ε stands for the empty string.
      {x} stands for zero or more occurrences of x.

      Type-Id is an identifier defined by a TypeDeclaration.

    Local variables =>
      "let" VarDeclaration "in" Exp "end"

      Note:
        The scope of the variable starts after it's declaration and lasts until the "end" keyword.

    Localally declared types =>
       "let" TypeDeclaration "in" Exp "end"

       Note:
         The scope of the type starts after it's declaration and lasts until the "end" keyword.
         This includes headers and bodies of any functions within the scope.

    Locally declared functions =>
      "let" FunctionDeclaration "in" Exp "end"

      Note:
        The scope of the function starts after it's declaration and lasts until the "end" keyword.
        This includes headers and bodies of any functions within the scope.

    Sequence => '(' Exp (';' Exp)* ')'
    NoValue => '(' ')' | "let" "in" "end"
    Negation => '-' Integer
    Arithmetic => Exp Op Exp
    Op => '=' | "<>" | '>' | '<' | ">=" | '<=' | '+' | '-' | '*' | '/' | '&' | '|'
*)

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
