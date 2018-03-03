%{

open Main

%}
%token <string>    VAL
%token IN NOT POINT COMMA SELECT FROM WHERE MINUS UNION AS
%token LT LE GT GE EQ NEQ
%token LPAREN RPAREN
%token EOL




%start main
%type <Main.expr > main
%%

main:
  | s EOL
;

s:
  | SELECT atts FROM rels WHERE cond
  | LPAREN S RPAREN MINUS LPAREN S RPAREN
  | LPAREN S RPAREN UNION LPAREN S RPAREN
;

atts:
  | attd COMMA atts
  | attd
;

attd:
  | att
  | att AS id
;

att:
  | id POINT id
;

id:
  | VAL
;

rels:
  | rel COMMA rels
  | rel
;

rel:
  | FILENAME id
  | LPAREN s RPAREN id
;

cond:
  | and_cond OR cond
  | and_cond

and_cond:
  | at_cond AND and_cond
  | at_cond
;

at_cond:
  | att EQ att
  | att LT att
  | att IN LPAREN s RPAREN
  | att NOT IN LPAREN s RPAREN
;
