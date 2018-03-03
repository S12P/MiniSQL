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
  | (['0'-'9']+ as i)         { VAL (int_of_string i) }
  | (['a'-'z''A'-'Z']+ as i)  { IDE i }
  | eof                       { EOF }
