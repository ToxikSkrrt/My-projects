
{
    open Parser
    exception Error of string
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
    | eof                       { EOF }

    | "//" [^ '\n']* '\n'       { Lexing.new_line lexbuf; token lexbuf }
    | "/*"                      { commentary lexbuf }
    | [' ' '\t' '\r']           { token lexbuf }
    | '\n'                      { Lexing.new_line lexbuf ; token lexbuf }
    
    | "And"                     { AND }
    | "Blue"                    { BLUE }
    | "Bool"                    { BOOL_TYPE }
    | "Color"                   { COLOR }
    | "Coord"                   { COORD }
    | "Cos"                     { COS }
    | "Draw"                    { DRAW }
    | "Else"                    { ELSE }
    | "False"                   { FALSE }
    | "Floor"                   { FLOOR }
    | "For"                     { FOR }
    | "Foreach"                 { FOREACH }
    | "From"                    { FROM }
    | "Green"                   { GREEN }
    | "Head"                    { HEAD }
    | "If"                      { IF }
    | "In"                      { IN }
    | "Int"                     { INT_TYPE }
    | "List"                    { LIST }
    | "Not"                     { NOT }
    | "Or"                      { OR }
    | "Pixel"                   { PIXEL }
    | "Print"                   { PRINT }
    | "Real"                    { REAL_TYPE }
    | "Real_of_int"             { REAL_OF_INT }
    | "Red"                     { RED }
    | "Set"                     { SET }
    | "Sin"                     { SIN }
    | "Step"                    { STEP }
    | "Tail"                    { TAIL }
    | "To"                      { TO }
    | "True"                    { TRUE }
    | "X"                       { X }
    | "Y"                       { Y }
    | "Pi"                      { PI }

    | "$<"                      { L_BLOCK }
    | ">$"                      { R_BLOCK }
    | "+"                       { ADD }
    | "-"                       { SUB }
    | "*"                       { MUL }
    | "/"                       { DIV }
    | "%"                       { MOD }
    | "="                       { EQ }
    | "<>"                      { DIFF }
    | "<="                      { LEQ }
    | ">="                      { GEQ }
    | "<"                       { LT }
    | ">"                       { GT }
    | ":"                       { AFFECT }
    | "::"                      { CONS }
    | "."                       { DOT }
    | "("                       { L_PAR }
    | ")"                       { R_PAR }
    | "["                       { L_BRK }
    | "]"                       { R_BRK }
    | ","                       { COMMA }
    | ";"                       { SEMICOLON }

    | ['a'-'z' 'A'-'Z'] (alphanum)* as s                            { ID(s) }
    | (digit)+ as s | "0x" (['0'-'9' 'A'-'F'] (alphanum)+) as s     { INT(try int_of_string s with Failure _ ->(let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%s'. It is not a valid integer" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) )) }
    | (digit)* "." (digit)* as s                                    { REAL(try float_of_string s with Failure _ -> raise (Error(s)) ) }
    | _ as s                                                        { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }

and commentary = parse
    | '\n'                      { Lexing.new_line lexbuf; commentary lexbuf }
    | "*/"                      { token lexbuf }
    | _                         { commentary lexbuf }