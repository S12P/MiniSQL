{
  open Parser;;
  exception Eof;;
}

rule token = parse
  | [' ' '\t' '\n']                                    { token lexbuf }
  | "SELECT"                                           { SELECT }
  | "FROM"                                             { FROM }
  | "WHERE"                                            { WHERE }
  | "MINUS"                                            { MINUS }
  | "UNION"                                            { UNION }
  | '('                                                { LPAREN }
  | ')'                                                { RPAREN }
  | ','                                                { COMMA }
  | '.'                                                { DOT }
  | "IN"                                               { IN }
  | "NOT"                                              { NOT }
  | "AND"                                              { AND }
  | "OK"                                               { OR }
  | '<'                                                { LT }
  | '='                                                { EQ }
  | "AS"                                               { AS }
  | '"'                                                { QUOTES }
  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as i   { VAL i }
  | ['A'-'Z''a'-'z''\\''-''_''0'-'9']+".csv" as i      { FILE i }
  | eof                                                { EOF }
