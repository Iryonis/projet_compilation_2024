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
%token <int> INT
%token <string> ID 
%token <float> REAL




%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }