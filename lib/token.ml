type pos = int

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

type source_value 

type token 
