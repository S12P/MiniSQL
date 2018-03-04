{
  open Parser;;
  exception Eof;;
}

rule token = parse
  | [' ' '\t']                { token lexbuf }
  | "SELECT"                  { SELECT }
  | "FROM"                    { FROM }
  | "WHERE"                   { WHERE }
  | "MINUS"                   { MINUS }
  | "UNION"                   { UNION }
  | '('                       { LPAREN }
  | ')'                       { RPAREN}
  | ','                       { COMMA}
  | '.'                       { POINT}
  | "IN"                      { IN }
  | "NOT"                     { NOT }
  | '<'                       { LT }
  | '>'                       { GT }
  | "<="                      { LE }
  | ">="                      { GE }
  | '='                       { EQ }
  | "AS"                      { AS }
  | (['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']+ as i)
                              { VAL i }
  | '\n'                       { EOL }
  | eof                       { EOF }
