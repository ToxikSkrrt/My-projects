open Util
open Ast

(* Codez ici la passe de renommage de termes.

   Cette passe permet de simplifier grandement la gestion des passes suivantes, et de l’interprétation, en assurant qu’un nom de variable n’est jamais réutilisé dans la même portée de déclaration.

   Pour cela, on va modifier le programme en, pour chaque nom, gardant un nombre correspondant son nombre de redéfinition, et en renommant l’occurence de chaque nom par un identifiant unique (son nom, suivi de son nombre d’occurence, avec un séparateur interdit dans le langage (pour empêcher les redéfinitions)).

   Comme seule la portée des variables est importante, dans deux blocs disjoints, il est possible de réutiliser un même nom.

   Pour obtenir ce résultat, il sera nécessaire de copier les environnement avant d’évaluer des sous-blocs (puisqu’en sortant d’un bloc, il est possible de continuer à utiliser un nom défini plus haut).

   Attention, l’interpréteur ne fonctionnera pas correctement en cas de redéfinition si vous n’effectuez pas correctement cette passe.*)

let rename_argument (argument : argument) (env : int Environment.t) =
   match argument with
   | Argument (id, typ, ann) ->
      (match Environment.get env id with
      | Some v ->
         (Environment.modify env id (v + 1);
         Argument (id ^ "#" ^ (string_of_int (v + 1)), typ, ann))
      | None ->
         (Environment.modify env id 0;
         Argument (id, typ, ann)))


let rec rename_expression (expression : expression) (env : int Environment.t) =
   match expression with
   | Variable (id, ann) ->
      (match Environment.get env id with
      | Some v ->
         if v = 0 then
            Variable (id, ann)
         else
            Variable (id ^ "#" ^ (string_of_int v), ann)
      | None ->
         Variable (id, ann))
   | Coord (e1, e2, ann) ->
      Coord (rename_expression e1 env, rename_expression e2 env, ann)
   | Color (e1, e2, e3, ann) ->
      Color (rename_expression e1 env, rename_expression e2 env, rename_expression e3 env, ann)
   | Pixel (e1, e2, ann) ->
      Pixel (rename_expression e1 env, rename_expression e2 env, ann)
   | Binary_operator (binop, e1, e2, ann) ->
      Binary_operator (binop, rename_expression e1 env, rename_expression e2 env, ann)
   | Unary_operator (unop, e, ann) ->
      Unary_operator (unop, rename_expression e env, ann)
   | Field_accessor (field, e, ann) ->
      Field_accessor (field, rename_expression e env, ann)
   | List (el, ann) ->
      let rec aux l acc =
         match l with
         | [] -> List (List.rev acc, ann)
         | h :: t -> aux t ((rename_expression h env) :: acc)
      in aux el []
   | Append (e1, e2, ann) ->
      Append (rename_expression e1 env, rename_expression e2 env, ann)
   | _ -> expression

let rec rename_statement (statement : statement) (env : int Environment.t) =
   match statement with
   | Declaration (id, typ, ann) ->
      (match Environment.get env id with
      | Some v ->
         (Environment.modify env id (v + 1);
         Declaration (id ^ "#" ^ (string_of_int (v + 1)), typ, ann))
      | None ->
         (Environment.modify env id 0;
         Declaration (id, typ, ann)))
   | Block (l, ann) ->
      let env_cpy = Environment.copy env in
      let rec aux li acc =
         match li with
         | [] ->
            Block (List.rev acc, ann)
         | h :: t ->
            aux t ((rename_statement h env_cpy) :: acc)
      in aux l []
   | IfThenElse (test, th, el, ann) ->
      let r_test = rename_expression test env in
      let env_th = Environment.copy env in
      let env_el = Environment.copy env in
      IfThenElse (r_test, rename_statement th env_th, rename_statement el env_el, ann)
   | For (id, e1, e2, e3, body, ann) ->
      let r_e1 = rename_expression e1 env in
      let r_e2 = rename_expression e2 env in
      let r_e3 = rename_expression e3 env in
      let env_cpy = Environment.copy env in
      (match Environment.get env_cpy id with
      | Some v ->
         (Environment.modify env_cpy id (v + 1);
         For (id ^ "#" ^ (string_of_int (v + 1)), r_e1, r_e2, r_e3, rename_statement body env_cpy, ann))
      | None ->
         (Environment.add env_cpy id 0;
         For (id, r_e1, r_e2, r_e3, rename_statement body env_cpy, ann)))
   | Foreach (id, test, body, ann) ->
      let r_test = rename_expression test env in
      let env_cpy = Environment.copy env in
      (match Environment.get env_cpy id with
      | Some v ->
         (Environment.modify env_cpy id (v + 1);
         Foreach (id ^ "#" ^ (string_of_int (v + 1)), r_test, rename_statement body env_cpy, ann))
      | None ->
         (Environment.modify env_cpy id 0;
         Foreach (id, r_test, rename_statement body env_cpy, ann)))
   | Affectation (e1, e2, ann) ->
      Affectation (rename_expression e1 env, rename_expression e2 env, ann)
   | Draw_pixel (e, ann) ->
      Draw_pixel (rename_expression e env, ann)
   | Print (e, ann) ->
      Print (rename_expression e env, ann)
   | _ ->
      statement

let renaming (program : program) =
  let name_counter = Environment.new_environment () in
  let Program (al, stmt) = program in
  let rec aux l acc =
   match l with
   | [] -> Program (List.rev acc, rename_statement stmt name_counter)
   | h :: t -> aux t ((rename_argument h name_counter) :: acc)
  in aux al []