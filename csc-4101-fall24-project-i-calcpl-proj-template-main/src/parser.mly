%{
open Ast
%}

%token <int> INT
%token <float> FLOAT (* NEW *)
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token GEQ (* NEW *)
%token TIMES
%token PLUS
%token MINUS (* NEW *)
%token DIV (* NEW *)
%token FPLUS (* NEW *)
%token FTIMES (* NEW *)
%token FMINUS (* NEW *)
%token FDIV (* NEW *)
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token COLON
%token INT_TYPE
%token BOOL_TYPE
%token FLOAT_TYPE (* NEW *)
%token EOF

%nonassoc IN
%nonassoc ELSE
%left GEQ LEQ (* NEW: Same precedence for comparison ops *)
%left PLUS MINUS FPLUS FMINUS (* NEW: Addition/subtraction group *)
%left TIMES DIV FTIMES FDIV (* NEW: Multiplication/division group *)

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| i = INT { Int i }
	| f = FLOAT { Float f } (* NEW *)
  	| x = ID { Var x }
  	| TRUE { Bool true }
  	| FALSE { Bool false }
  	
    (* Comparison Operators *)
  	| e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
    | e1 = expr; GEQ; e2 = expr { Binop (Geq, e1, e2) } (* NEW *)

    (* Int Operators *)
  	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
    | e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) } (* NEW *)
    | e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) } (* NEW *)

    (* Float Operators *)
    | e1 = expr; FTIMES; e2 = expr { Binop (FMult, e1, e2) } (* NEW *)
    | e1 = expr; FPLUS; e2 = expr { Binop (FAdd, e1, e2) } (* NEW *)
    | e1 = expr; FMINUS; e2 = expr { Binop (FSub, e1, e2) } (* NEW *)
    | e1 = expr; FDIV; e2 = expr { Binop (FDiv, e1, e2) } (* NEW *)

  	| LET; x = ID; COLON; t = typ; EQUALS; e1 = expr; IN; e2 = expr
		{ Let (x, t, e1, e2) }
  	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  	| LPAREN; e=expr; RPAREN {e}
	;

typ: 
	| INT_TYPE { TInt }
	| BOOL_TYPE { TBool }
    | FLOAT_TYPE { TFloat } (* NEW *)