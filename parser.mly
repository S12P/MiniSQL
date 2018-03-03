%{

open Expr

%}
/* ordre de priorité et de précedence*/
%token <int>    VAL
%token <string> IDE
%token IN
%token SUB ADD
%token MUL
%token LT LE GT GE EQ NEQ
%token LPAREN RPAREN
%token LET REC
%token IF THEN ELSE TRY WITH RAISE EX
%token FUN ARROW
%token EOL
%token EOF


%nonassoc EOL
%nonassoc EOF
%nonassoc TEMP
%nonassoc LET REC FUN EQ
%right ARROW
%nonassoc VAL IDE
%nonassoc LPAREN RPAREN NEQ GE GT LE LT
%nonassoc IF THEN ELSE TRY WITH EX
%right IN
%left ADD SUB
%left MUL
%nonassoc UMINUS


%start main
%type <Expr.expr list> main
%%

main:
  | expr EOL main { $1::$3 }
  | EOF           { [] }
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
