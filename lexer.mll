{
  open Parser;;
  exception Eof;;
}

rule token = parse
  | [' ' '\t' '\n']                { token lexbuf }
  | "SELECT"                  { SELECT }
  | "FROM"                    { FROM }
  | "WHERE"                   { WHERE }
  | "MINUS"                   { MINUS }
  | "UNION"                   { UNION }
  | '('                       { LPAREN }
  | ')'                       { RPAREN }
  | ','                       { COMMA }
  | '.'                       { DOT }
  | "IN"                      { IN }
  | "NOT"                     { NOT }
  | '<'                       { LT }
  | '='                       { EQ }
  | "AS"                      { AS }
  | ['A'-'Z''a'-'z']* as i
                              { VAL i }
  | eof                       { EOF }