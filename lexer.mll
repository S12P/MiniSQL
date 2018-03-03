{
  open Parser;;
  exception Eof;;
}

rule token = parse
  | [' ' '\t' '\n']           { token lexbuf }
  | "SELECT"                  { SELECT }
  | "FROM"                    { FROM }
  | "WHERE"                   { WHERE }
  | "MINUS"                   { MINUS }
  | "UNION"                   { UNION }
  | "("                       { LPAREN }
  | ")"                       { RPAREN}
  | ","                       { COMMA}
  | "."                       { POINT}
  | "IN"                      { IN }
  | "NOT"                     { NOT }
  | "<"                       { LT }
  | ">"                       { GT }
  | "<="                      { LE }
  | ">="                      { GE }
  | "="                       { EQ }
  | "AS"                      { AS }
  | [A-Za-z][A-Za-z0-9_]+     { VAL i }
  | eof                       { EOF }
