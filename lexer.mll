{
  open Parser;;
  exception Eof;;
}

rule token = parse
  | [' ' '\t' '\n']           { token lexbuf }
  | "SELECT"                  {  select }
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
  | "AS"                      { AS }
  | (['0'-'9']+ as i)         { VAL (int_of_string i) }
  | (['a'-'z''A'-'Z']+ as i)  { IDE i }
  | eof                       { EOF }
