%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}


%token EOF
%token AND
%token BLUE
%token BOOL
%token COLOR
%token COORD
%token COS 
%token DRAW
%token ELSE
%token FALSE
%token FLOOR
%token FOR
%token FOREACH
%token FROM
%token GREEN
%token HEAD
%token IF
%token IN
%token INTTYPE
%token LIST
%token NOT
%token OR
%token PIXEL
%token PRINT
%token REALTYPE
%token REAL_OF_INT
%token RED
%token SET
%token SIN
%token STEP
%token TAIL
%token TO 
%token TRUE
%token X
%token Y
%token PI 
%token DOLLAR_LESS
%token DOLLAR_GREATER
%token ADD
%token SUB
%token MUL 
%token DIV 
%token MOD 
%token EQ 
%token NEQ 
%token LT 
%token GT 
%token LEQ 
%token GEQ 
%token COLON
%token DOUBLE_COLON
%token DOT
%token COMMA
%token SEMICOLON
%token LPAR
%token RPAR
%token L_SQ_BRK
%token R_SQ_BRK
%token HASH(*not sure*)
%token <int> INT
%token <string> ID 
%token <float> REAL


%left ADD SUB
%left MUL DIV MOD

%right DOT DOUBLE_COLON 

%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }


expression:
|L_SQ_BRK i = INT R_SQ_BRK{ Const_int(i,Annotation.create $loc) }
|L_SQ_BRK f = REAL R_SQ_BRK{ Const_float(f,Annotation.create $loc) }
| f = REAL { Const_float(float.PI,Annotation.create $loc) }
|b = BOOL { Const_bool(b,Annotation.create $loc) }
|L_SQ_BRK x = ID R_SQ_BRK { Var(x,Annotation.create $loc) }
|COORD LPAR HASH x = expression HASH COMMA HASH y = expression HASH RPAR { Coord(x,y,Annotation.create $loc) }
|COLOR LPAR HASH r = expression HASH COMMA HASH g = expression HASH COMMA HASH b = expression HASH RPAR { Color(r,g,b,Annotation.create $loc) }
|PIXEL LPAR HASH x = expression HASH COMMA HASH y = expression HASH RPAR { Pixel(x,y,Annotation.create $loc) }
|HASH x = expression HASH HASH op = binop HASH  HASH y = expression HASH { Binop(op,x,y,Annotation.create $loc) }


%inline binop:
|ADD {Plus}
|SUB {Minus}
|MUL {Times}
|DIV {Div}
|MOD {Rem}
|AND {And}
|OR  {Or}
|EQ  {Equal}
|NEQ {Diff}
|LT  {Lt}
|GT  {Gt}
|LEQ {Leq}
|GEQ {Geq}