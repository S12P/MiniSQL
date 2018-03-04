%{

open DataType

%}

%token <string> VAL
%token IN NOT POINT COMMA SELECT FROM WHERE MINUS UNION AS
%token LT LE GT GE EQ NEQ AND OR
%token LPAREN RPAREN
%token EOL EOF




%start main
%type <DataType.requete> main
%type <DataType.cond> cond and_cond at_cond
%type <DataType.idstring> att
%type <DataType.idstring list> atts attd
%type <string> id
%type <string list> rels rel
%%

main:
  | s EOF                                          { $1 }
;

s:
  | SELECT atts FROM rels WHERE cond               { Where ({DataType.col = $2 ; DataType.table = $4; DataType.cond = $6}) }
  | LPAREN s RPAREN MINUS LPAREN s RPAREN          { Minus ($2, $6) }
  | LPAREN s RPAREN UNION LPAREN s RPAREN          { Union ($2, $6) }
;

atts:
  | attd COMMA atts                                { $1 @ $3 }
  | attd                                           { $1 }
;

attd:
  | att                                            { [$1] }
  | att AS id                                      { [$1] }
;

att:
  | id POINT id                                    { ID( $1, $3) }
;

id:
  | VAL                                            { $1 }
;

rels:
  | rel COMMA rels                                 { $1 @ $3 }
  | rel                                            { $1 }
;

rel:
  | /*FILENAME*/ id                                { [$1] } 
  /*| LPAREN s RPAREN id*/
;

cond:
  | and_cond OR cond                               { Or($1, $3) }
  | and_cond                                       { $1 }

and_cond:
  | at_cond AND and_cond                           { And($1, $3) }
  | at_cond                                        { $1 }
;

at_cond:
  | att EQ att                                     { Rel($1, Eq, $3) }
  | att LT att                                     { Rel($1, Lt, $3) }
  /*| att IN LPAREN s RPAREN
  | att NOT IN LPAREN s RPAREN */
;
