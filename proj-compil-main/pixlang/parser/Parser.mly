%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}


%token EOF
%token AND
%token BLUE
%token BOOL_TYPE
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
%token INT_TYPE
%token LIST
%token NOT
%token OR
%token PIXEL
%token PRINT
%token REAL_TYPE
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
%token L_BLOCK
%token R_BLOCK
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token EQ
%token DIFF
%token LEQ
%token GEQ
%token LT
%token GT
%token AFFECT
%token CONS
%token DOT
%token L_PAR
%token R_PAR
%token L_BRK
%token R_BRK
%token COMMA
%token SEMICOLON
%token <string> ID
%token <int> INT
%token <float> REAL


%right ELSE
%left OR
%left AND
%nonassoc EQ DIFF LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%right DOT
%right CONS
%nonassoc NOT HEAD TAIL FLOOR REAL_OF_INT COS SIN

%start <program> main
%%

main:
| p = program EOF { p }

type_expression:
| INT_TYPE { Type_int }
| REAL_TYPE { Type_real }
| BOOL_TYPE { Type_bool }
| COORD { Type_coord }
| COLOR { Type_color }
| PIXEL { Type_pixel }
| LIST L_PAR t = type_expression R_PAR { Type_list (t) }

%inline binary_operator:
| ADD { Plus }
| SUB { Minus }
| MUL { Times }
| DIV { Div }
| MOD { Rem }
| AND { And }
| OR { Or }
| EQ { Equal }
| DIFF { Diff }
| LT { Lt }
| GT { Gt }
| LEQ { Leq }
| GEQ { Geq }

%inline unary_operator:
| SUB { Opposite }
| NOT { Not }
| HEAD { Head }
| TAIL { Tail }
| FLOOR { Floor }
| REAL_OF_INT { Real_of_int }
| COS { Cos }
| SIN { Sin }

%inline field_accessor:
| COLOR { Color_field }
| COORD { Coord_field }
| X { X_field }
| Y { Y_field }
| RED { Red_field }
| GREEN { Green_field }
| BLUE { Blue_field }

expression:
| i = INT { Const_int (i, Annotation.create $loc) }
| r = REAL { Const_real (r, Annotation.create $loc) } 
| PI { Const_real (Float.pi, Annotation.create $loc) }
| TRUE { Const_bool (true, Annotation.create $loc) }
| FALSE { Const_bool (false, Annotation.create $loc) }
| id = ID { Variable (id, Annotation.create $loc) }
| COORD L_PAR e1 = expression COMMA e2 = expression R_PAR { Coord (e1, e2, Annotation.create $loc) }
| COLOR L_PAR e1 = expression COMMA e2 = expression COMMA e3 = expression R_PAR { Color (e1, e2, e3, Annotation.create $loc) }
| PIXEL L_PAR e1 = expression COMMA e2 = expression R_PAR { Pixel (e1, e2, Annotation.create $loc) }
| e1 = expression b = binary_operator e2 = expression { Binary_operator (b, e1, e2, Annotation.create $loc) }
| u = unary_operator e = expression { Unary_operator (u, e, Annotation.create $loc) }
| e = expression DOT f = field_accessor { Field_accessor (f, e, Annotation.create $loc) }
| L_BRK el = expression_list R_BRK { List (List.rev el, Annotation.create $loc) }
| e1 = expression CONS e2 = expression { Append (e1, e2, Annotation.create $loc) }
| L_PAR e = expression R_PAR { e }

statement:
| SET L_PAR e1 = expression COMMA e2 = expression R_PAR { Affectation (e1, e2, Annotation.create $loc) }
| t = type_expression AFFECT id = ID { Declaration (id, t, Annotation.create $loc) }
| L_BLOCK sl = statement_list R_BLOCK { Block (List.rev sl, Annotation.create $loc) }
| IF L_PAR e = expression R_PAR s = statement ifife = ife { IfThenElse (e, s, ifife, Annotation.create $loc) }
| FOR id = ID FROM e1 = expression TO e2 = expression STEP e3 = expression s = statement { For (id, e1, e2, e3, s, Annotation.create $loc) }
| FOREACH id = ID IN e = expression s = statement { Foreach (id, e, s, Annotation.create $loc) }
| DRAW L_PAR e = expression R_PAR { Draw_pixel (e, Annotation.create $loc) }
| PRINT L_PAR e = expression R_PAR { Print (e, Annotation.create $loc) }
| { Nop }

%inline ife:
| ELSE s = statement { s }
| { Nop }
%prec ELSE

argument:
| t = type_expression AFFECT id = ID { Argument (id, t, Annotation.create $loc) }

program:
| LT al = argument_list GT s = statement { Program (List.rev al, s) }
| s = statement { Program ([], s) }

argument_list:
| a = argument { [a] }
| al = argument_list SEMICOLON a = argument { a :: al }
| { [] }

statement_list:
| s = statement { [s] }
| sl = statement_list SEMICOLON s = statement { s :: sl }
// | { [] }

expression_list:
| e = expression { [e] }
| el = expression_list COMMA e = expression { e :: el }
| { [] }
