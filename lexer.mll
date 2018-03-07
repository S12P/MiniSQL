{
  open Parser;;
  exception Eof;;
}

rule token = parse
  | [' ' '\t' '\n']                                { token lexbuf }
  | "SELECT"                                       { SELECT }
  | "FROM"                                         { FROM }
  | "WHERE"                                        { WHERE }
  | "MINUS"                                        { MINUS }
  | "UNION"                                        { UNION }
  | '('                                            { LPAREN }
  | ')'                                            { RPAREN }
  | ','                                            { COMMA }
  | '.'                                            { DOT }
  | "IN"                                           { IN }
  | "NOT"                                          { NOT }
  | '<'                                            { LT }
  | '='                                            { EQ }
  | "AS"                                           { AS }
  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']* as i                         { VAL i }
  | ['A'-'Z''a'-'z''\\''-''_''0'-'9']+".csv" as i   { FILE i }
  | eof                                            { EOF }
  