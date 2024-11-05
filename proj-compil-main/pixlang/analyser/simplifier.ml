open Ast
(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points (entre autres) qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (pixels, coordonnées et couleurs) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)

let get_ann_expr (expr : Ast.expression) =
  match expr with
  | Const_int (_, ann)
  | Const_real (_, ann)
  | Const_bool (_, ann)
  | Coord (_, _, ann)
  | Color (_, _, _, ann)
  | Pixel (_, _, ann)
  | Variable (_, ann)
  | Binary_operator (_, _, _, ann)
  | Unary_operator (_, _, ann)
  | Field_accessor (_, _, ann)
  | List (_, ann)
  | Append (_, _, ann) -> ann

let rec simplify_expr (expr : Ast.expression) =
  match expr with 
  | Coord (x, y, annotation) -> Coord (simplify_expr x, simplify_expr y, annotation)
  | Color (r, g, b, annotation) -> Color (simplify_expr r, simplify_expr g, simplify_expr b, annotation)
  | Pixel (p, c, annotation) -> Pixel (simplify_expr p , simplify_expr c, annotation)
  | Binary_operator (op, e1, e2, annotation) ->
    let s1 = simplify_expr e1 in 
    let s2 = simplify_expr e2 in
    (match op with
    | Plus ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_int (v1 + v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_real(v1 +. v2, annotation)
      | Const_int (v, _), Coord (Const_int (x, _), Const_int (y, _), _)
      | Coord (Const_int (x, _), Const_int (y, _), _), Const_int (v,_) -> Coord (Const_int (x + v, annotation), Const_int(y + v, annotation), annotation)
      | Const_int (v,_), Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) 
      | Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) , Const_int (v,_) -> Color (Const_int(r + v, annotation), Const_int(g + v,  annotation), Const_int(b + v,  annotation),annotation)
      | _ ->
        let ann1 = get_ann_expr s1 in
        let ann2 = get_ann_expr s2 in
        match (Annotation.get_type ann1, Annotation.get_type ann2) with 
        | Some (Type_int), Some (Type_real) -> let new_ann1 = Annotation.create (Annotation.get_pos ann1) in Annotation.set_type new_ann1 Type_real ; Binary_operator (Plus, Unary_operator (Real_of_int, s1, Annotation.create (Annotation.get_pos new_ann1)), s2, annotation)
        | Some (Type_real), Some (Type_int) -> let new_ann2 = Annotation.create (Annotation.get_pos ann2) in Annotation.set_type new_ann2 Type_real ; Binary_operator (Plus, s1, Unary_operator (Real_of_int, s2, Annotation.create (Annotation.get_pos new_ann2)), annotation)
        | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Minus ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_int (v1 - v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_real(v1 -. v2, annotation)
      | Const_int (v, _), Coord (Const_int (x, _), Const_int (y, _), _)
      | Coord (Const_int (x, _), Const_int (y, _), _), Const_int (v,_) -> Coord (Const_int (x - v, annotation), Const_int(y - v, annotation), annotation)
      | Const_int (v,_), Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) 
      | Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) , Const_int (v,_) -> Color (Const_int(r - v, annotation), Const_int(g - v,  annotation), Const_int(b - v,  annotation),annotation)
      | _ ->
        let ann1 = get_ann_expr s1 in
        let ann2 = get_ann_expr s2 in
        match (Annotation.get_type ann1, Annotation.get_type ann2) with 
        | Some (Type_int), Some (Type_real) -> let new_ann1 = Annotation.create (Annotation.get_pos ann1) in Annotation.set_type new_ann1 Type_real ; Binary_operator (Minus, Unary_operator (Real_of_int, s1, Annotation.create (Annotation.get_pos new_ann1)), s2, annotation)
        | Some (Type_real), Some (Type_int) -> let new_ann2 = Annotation.create (Annotation.get_pos ann2) in Annotation.set_type new_ann2 Type_real ; Binary_operator (Minus, s1, Unary_operator (Real_of_int, s2, Annotation.create (Annotation.get_pos new_ann2)), annotation)
        | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Times ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_int (v1 * v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_real(v1 *. v2, annotation)
      | Const_int (v, _), Coord (Const_int (x, _), Const_int (y, _), _)
      | Coord (Const_int (x, _), Const_int (y, _), _), Const_int (v,_) -> Coord (Const_int (x * v, annotation), Const_int(y * v, annotation), annotation)
      | Const_int (v,_), Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) 
      | Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) , Const_int (v,_) -> Color (Const_int(r * v, annotation), Const_int(g * v,  annotation), Const_int(b * v,  annotation),annotation)
      | _ ->
        let ann1 = get_ann_expr s1 in
        let ann2 = get_ann_expr s2 in
        match (Annotation.get_type ann1, Annotation.get_type ann2) with 
        | Some (Type_int), Some (Type_real) -> let new_ann1 = Annotation.create (Annotation.get_pos ann1) in Annotation.set_type new_ann1 Type_real ; Binary_operator (Times, Unary_operator (Real_of_int, s1, Annotation.create (Annotation.get_pos new_ann1)), s2, annotation)
        | Some (Type_real), Some (Type_int) -> let new_ann2 = Annotation.create (Annotation.get_pos ann2) in Annotation.set_type new_ann2 Type_real ; Binary_operator (Times, s1, Unary_operator (Real_of_int, s2, Annotation.create (Annotation.get_pos new_ann2)), annotation)
        | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Div ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_int (v1 / v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_real(v1 /. v2, annotation)
      | Const_int (v, _), Coord (Const_int (x, _), Const_int (y, _), _)
      | Coord (Const_int (x, _), Const_int (y, _), _), Const_int (v,_) -> Coord (Const_int (x / v, annotation), Const_int(y / v, annotation), annotation)
      | Const_int (v,_), Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) 
      | Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) , Const_int (v,_) -> Color (Const_int(r / v, annotation), Const_int(g / v,  annotation), Const_int(b / v,  annotation),annotation)
      | _ ->
        let ann1 = get_ann_expr s1 in
        let ann2 = get_ann_expr s2 in
        match (Annotation.get_type ann1, Annotation.get_type ann2) with 
        | Some (Type_int), Some (Type_real) -> let new_ann1 = Annotation.create (Annotation.get_pos ann1) in Annotation.set_type new_ann1 Type_real ; Binary_operator (Div, Unary_operator (Real_of_int, s1, Annotation.create (Annotation.get_pos new_ann1)), s2, annotation)
        | Some (Type_real), Some (Type_int) -> let new_ann2 = Annotation.create (Annotation.get_pos ann2) in Annotation.set_type new_ann2 Type_real ; Binary_operator (Div, s1, Unary_operator (Real_of_int, s2, Annotation.create (Annotation.get_pos new_ann2)), annotation)
        | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Rem ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_int (v1 mod v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_real(mod_float v1 v2, annotation)
      | Const_int (v, _), Coord (Const_int (x, _), Const_int (y, _), _)
      | Coord (Const_int (x, _), Const_int (y, _), _), Const_int (v,_) -> Coord (Const_int (x mod v, annotation), Const_int(y mod v, annotation), annotation)
      | Const_int (v,_), Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) 
      | Color (Const_int(r, _), Const_int(g, _), Const_int(b, _), _) , Const_int (v,_) -> Color (Const_int(r mod v, annotation), Const_int(g mod v,  annotation), Const_int(b mod v,  annotation),annotation)
      | _ ->
        let ann1 = get_ann_expr s1 in
        let ann2 = get_ann_expr s2 in
        match (Annotation.get_type ann1, Annotation.get_type ann2) with 
        | Some (Type_int), Some (Type_real) -> let new_ann1 = Annotation.create (Annotation.get_pos ann1) in Annotation.set_type new_ann1 Type_real ; Binary_operator (Rem, Unary_operator (Real_of_int, s1, Annotation.create (Annotation.get_pos new_ann1)), s2, annotation)
        | Some (Type_real), Some (Type_int) -> let new_ann2 = Annotation.create (Annotation.get_pos ann2) in Annotation.set_type new_ann2 Type_real ; Binary_operator (Rem, s1, Unary_operator (Real_of_int, s2, Annotation.create (Annotation.get_pos new_ann2)), annotation)
        | _ -> Binary_operator (op, s1, s2, annotation)) 
    | And ->
      (match s1, s2 with
      | Const_bool (v1, _), Const_bool (v2, _) -> Const_bool (v1 && v2, annotation)
      | _ -> Binary_operator (op, s1, s2, annotation))
    | Or ->
      (match s1, s2 with
      | Const_bool (v1, _), Const_bool (v2, _) -> Const_bool (v1 || v2, annotation)
      | _ -> Binary_operator (op, s1, s2, annotation))
    | Equal ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_bool (v1 = v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_bool (v1 = v2, annotation)
      | Const_bool (v1, _), Const_bool (v2, _) -> Const_bool (v1 = v2, annotation)
      | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Diff ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_bool (not(v1 = v2), annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_bool (not(v1 = v2), annotation)
      | Const_bool (v1, _), Const_bool (v2, _) -> Const_bool (not(v1 = v2), annotation)
      | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Lt ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_bool (v1 < v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_bool (v1 < v2, annotation)
      | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Gt ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_bool (v1 > v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_bool (v1 > v2, annotation)
      | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Leq ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_bool (v1 <= v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_bool (v1 <= v2, annotation)
      | _ -> Binary_operator (op, s1, s2, annotation)) 
    | Geq ->
      (match s1, s2 with
      | Const_int (v1, _), Const_int (v2, _) -> Const_bool (v1 >= v2, annotation)
      | Const_real (v1, _), Const_real(v2, _) -> Const_bool (v1 >= v2, annotation)
      | _ -> Binary_operator (op, s1, s2, annotation)))
  | Unary_operator (op, e, annotation) ->
    let s = simplify_expr e in
    (match op with
    | Opposite ->
      (match s with
      | Const_int (v, _) -> Const_int (0 - v, annotation)
      | Const_real (v, _) -> Const_real (0. -. v, annotation)
      | _ -> Unary_operator (op, s, annotation))
    | Not ->
      (match s with
      | Const_bool (v, _) -> Const_bool (not(v), annotation)
      | _ -> Unary_operator (op, s, annotation))
    | Head ->
      (match s with 
      | List (el, _) -> 
        (match el with
        | [] -> expr
        | h :: _ -> 
          (match h with
          | Const_int (v, _) -> Const_int (v, annotation)
          | Const_real (v, _) -> Const_real (v, annotation)
          | Const_bool (v, _) -> Const_bool (v, annotation)
          | _ -> simplify_expr h))
        | _ -> Unary_operator (op, s, annotation))
    | Tail ->
      (match s with 
      | List (el, ann) -> 
        let rec aux l acc test =
          match l with
          | [] -> List (List.rev acc, ann)
          | h :: t ->
            if test = false then
              aux t acc true
            else
              aux t ((simplify_expr h) :: acc) test
          in aux el [] false
      | _ -> Unary_operator (op, s, annotation))
    | Floor ->
      (match s with 
      | Const_real (v, _) -> Const_int (int_of_float (v) ,annotation)
      | Unary_operator (Real_of_int, n, _) -> simplify_expr n 
      | _ -> Unary_operator (op, s, annotation))
    | Real_of_int ->
      (match s with 
      | Const_int (v, _) -> Const_real (float_of_int (v) ,annotation)
      | _ -> Unary_operator (op, s, annotation))
    | Cos -> 
      (match s with
      | Const_real (v, _) -> Const_real (cos (v) ,annotation)
      | _ -> Unary_operator (op, s, annotation))
    | Sin -> 
      (match s with
      | Const_real (v, _) -> Const_real (sin (v) ,annotation)
      | _ -> Unary_operator (op, s, annotation)))
  | Field_accessor (f, e, annotation) ->
    let se = simplify_expr e in
    (match f with
    | Color_field ->
      (match se with
      | Pixel (_, colorf, _) -> simplify_expr colorf
      | _ -> Field_accessor (f, se, annotation))
    | Coord_field ->
      (match se with
      | Pixel (coordf, _, _) -> simplify_expr coordf
      | _ -> Field_accessor (f, se, annotation))
    | X_field ->
      (match se with
      | Pixel (coordf, _, _) -> 
        (match coordf with 
        | Coord (x, _, _) -> simplify_expr x
        | _ -> expr)
      | _ -> Field_accessor (f, se, annotation))
    | Y_field ->
    (match se with
    | Pixel (coordf, _, _) -> 
      (match coordf with 
      | Coord (_, y, _) -> simplify_expr y
      | _ -> expr)
    | _ -> Field_accessor (f, se, annotation))
    | Blue_field ->
      (match se with
      | Pixel (coordf, _, _) -> 
        (match coordf with 
        | Color (_, _, b, _) -> simplify_expr b
        | _ -> expr)
      | _ -> Field_accessor (f, se, annotation))
    | Red_field ->
      (match se with
      | Pixel (coordf, _, _) -> 
        (match coordf with 
        | Color (r, _, _, _) -> simplify_expr r
        | _ -> expr)
      | _ -> Field_accessor (f, se, annotation))
    | Green_field ->
      (match se with
      | Pixel (coordf, _, _) -> 
        (match coordf with 
        | Color (_, g, _, _) -> simplify_expr g
        | _ -> expr)
      | _ -> Field_accessor (f, se, annotation)))
  | List (el, annotation) -> 
    let rec aux l acc =
      match l with
      | [] -> List (List.rev acc, annotation)
      | h :: t -> aux t ((simplify_expr h) :: acc)
    in aux el []
  | Append (e, li, annotation) ->
    (match li with 
    | List (lis, _) -> List ((simplify_expr e) :: lis, annotation)
    | _ -> Append (simplify_expr e, li, annotation))
  | _ -> expr (*Const_int, Const_real, Const_bool, Variable *)


let rec simplify_stat (stat : Ast.statement) =
  match stat with 
  | Affectation (e1, e2, annotation) -> Affectation (simplify_expr e1, simplify_expr e2, annotation)
  | Block (li, annotation) ->
    let rec aux l acc =
      match l with
      | [] -> Block (List.rev acc, annotation)
      | h :: t -> aux t ((simplify_stat h) :: acc)
    in aux li []
  | IfThenElse (cond, st1, st2, annotation) ->
    (match cond with
    |Const_bool(c, _) -> if c then simplify_stat st1
                          else simplify_stat st2
    | _ -> IfThenElse(cond, simplify_stat st1, simplify_stat st2, annotation))
  | For (str, e1, e2 , e3, s, annotation) ->
    let se1 = simplify_expr e1 in
    let se2 = simplify_expr e2 in
    let se3 = simplify_expr e3 in
    (match se1, se2, se3 with
    | Const_int (v1, ann), Const_int (v2, _), Const_int (v3, _) ->
      if v1 + v3 > v2 then
        Block ((Declaration (str, Type_int, annotation)) :: Affectation (Variable (str, annotation), Const_int (v1, ann), annotation) :: [simplify_stat s], annotation)
      else
        if v1 > v2 then
          Block ([], annotation)
        else
          For (str, se1, se2, se3, simplify_stat s, annotation)
    | Const_real (v1, ann), Const_real (v2, _), Const_real (v3, _) ->
      if v1 +. v3 > v2 then
        Block ((Declaration (str, Type_real, annotation)) :: Affectation (Variable (str, annotation), Const_real (v1, ann), annotation) :: [simplify_stat s], annotation)
      else
        if v1 > v2 then
          Block ([], annotation)
        else
          For (str, se1, se2, se3, simplify_stat s, annotation)
    | _ -> stat)
  | Foreach (id, e, s, annotation) ->
    (let se = simplify_expr e in
    match se with
    | List ([], _) -> Block ([], annotation)
    | List (el, ann) ->
      if List.length el = 1 then
        let first_elem = simplify_expr (List.hd el) in
        let typ = Annotation.get_type ann in
        match typ with
        | Some (Type_list v) -> Block ((Declaration (id, v, ann) :: Affectation (Variable (id, ann), first_elem, ann) :: [simplify_stat s], annotation))
        | _ -> Block ((Declaration (id, Type_generic, ann) :: Affectation (Variable (id, ann), first_elem, ann) :: [simplify_stat s], annotation))
      else
        let rec aux l acc =
          match l with
          | [] -> Foreach (id, List (List.rev acc, ann), simplify_stat s, annotation)
          | h :: t -> aux t ((simplify_expr h) :: acc)
        in aux el []
    |_ -> stat)
  | Draw_pixel (e, annotation) -> Draw_pixel (simplify_expr e, annotation)
  | Print (e, annotation) -> Print (simplify_expr e, annotation)
  | _ -> stat (* Nop, Declaration *)

  
let simplifier (program : Ast.program) =
  let Program (al, stmt) = program in Program (al, simplify_stat stmt)