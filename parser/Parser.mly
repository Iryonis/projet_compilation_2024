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
%token BLOCKIOPEN
%token BLOCKCLOSE
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
%token <int> INT
%token <string> ID 
%token <float> REAL


%left ADD SUB
%left MUL DIV MOD

%right DOT DOUBLE_COLON 

%start <program> main
%%

main:
|p = program EOF { Progam p }
|e = expression EOF { Expression e }
| EOF { Program([],Block([],Annotation.create $loc)) }

program:
|LT listOfArgs = argumentList GT stmt = statement { Program(listOfArgs,stmt) }
|stmt = statement { Program([],stmt) }


argument:
|t = Type COLON id = ID { [Argument(id,t,Annotation.create $loc)] }

argumentList:
|arg = argument { [arg] }
|arg = argument COMMA argList = argumentList { arg::argList }

statement:
| {}
(*to do*)

expression:
|L_SQ_BRK i = INT R_SQ_BRK{ Const_int(i,Annotation.create $loc) }
|L_SQ_BRK f = REAL R_SQ_BRK{ Const_float(f,Annotation.create $loc) }
|f = REAL { Const_float(Float.pi,Annotation.create $loc) }
|b = BOOL { Const_bool(b,Annotation.create $loc) }
|L_SQ_BRK x = ID R_SQ_BRK { Var(x,Annotation.create $loc) }
|COORD LPAR x = expression COMMA y = expression RPAR { Coord(x,y,Annotation.create $loc) }
|COLOR LPAR r = expression COMMA g = expression COMMA b = expression  RPAR { Color(r,g,b,Annotation.create $loc) }
|PIXEL LPAR x = expression COMMA y = expression RPAR { Pixel(x,y,Annotation.create $loc) }
|expr1 = expression op = binop expr2 = expression { Binop(op, expr1, expr2,Annotation.create $loc) }
|op = unop expr1 = expression { Unop(op, expr1,Annotation.create $loc) }
|expr1 = expression DOT f = field { Field_accessor(f, expr1,Annotation.create $loc) }
|L_SQ_BRK exprList = expressionList R_SQ_BRK { List(exprList,Annotation.create $loc) }
|expr1 = expression DOUBLE_COLON expr2 = expression { Cons(expr1,expr2,Annotation.create $loc) }
|LPAR expr = expression RPAR { expr }

expressionList:
|expr = expression { [expr] }
|expr = expression COMMA exprList = expressionList { expr::exprList }


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


%inline Type:
|INTTYPE { Type_int }
|REALTYPE { Type_real }
|BOOL { Type_bool }
|COORD { Type_coord }
|COLOR { Type_color }
|PIXEL { Type_pixel }
|LIST LPAR t = Type RPAR { Type_list(t) }

%inline unop:
|SUB {Opposite}
|NOT {Not}
|HEAD {Head}
|TAIL {Tail}
|FLOOR {Floor}
|REAL_OF_INT {Real_of_int}
|SIN {Sin}
|COS {Cos}

%inline field:
|COLOR {Color_field}
|COORD {Coord_field}
|X {X_field}
|Y {Y_field}
|RED {Red_field}
|GREEN {Green_field}
|BLUE {Blue_field}
