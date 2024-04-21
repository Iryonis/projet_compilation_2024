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
open Ast

let simplifier (program : Ast.program) = program

let rec simplify_expression expr =
  match expr with
  | Unary_operator (op, e, annotation) -> (
      let simplified_e = simplify_expression e in
      match simplified_e with
      | Const_int (i, _) -> (
          match op with
          | Opposite -> Const_int (-i, annotation)
          | Real_of_int -> Const_real (float_of_int i, annotation)
          | _ -> failwith "undefined")
      | Const_real (r, _) -> (
          match op with
          | Opposite -> Const_real (-.r, annotation)
          | Floor -> Const_int (int_of_float r, annotation)
          | Cos -> Const_real (Float.cos r, annotation)
          | Sin -> Const_real (Float.sin r, annotation)
          | _ -> failwith "undefined")
      | Const_bool (b, _) -> (
          match op with
          | Not -> Const_bool (not b, annotation)
          | _ -> failwith "undefined")
      | _ -> expr)
  | Binary_operator (op, e1, e2, annotation) -> (
    let simplified_e1 = simplify_expression e1 in
    match simplified_e1 with 
    | Const_int (i1, _) -> (
      let simplified_e2 = simplify_expression e2 in
      match simplified_e2 with
      | Const_int (i2, _) -> (
        match op with
        | Plus -> Const_int (i1 + i2, annotation)
        | Minus -> Const_int (i1 - i2, annotation)
        | Times -> Const_int (i1 * i2, annotation)
        | Div -> Const_int (i1 / i2, annotation)
        | Rem -> Const_int (i1 mod i2, annotation)
        | Equal -> Const_bool (i1 = i2, annotation)
        | Diff -> Const_bool (i1 <> i2, annotation)
        | Lt -> Const_bool (i1 < i2, annotation)
        | Gt -> Const_bool (i1 > i2, annotation)
        | Leq -> Const_bool (i1 <= i2, annotation)
        | Geq -> Const_bool (i1 >= i2, annotation)
        | _ -> failwith "undefined")
      | Const_real (f2, _) -> (
        match op with
        | Add -> Const_real (float_of_int i1 +. f2, annotation)
        | Sub -> Const_real (float_of_int i1 -. f2, annotation)
        | Mul -> Const_real (float_of_int i1 *. f2, annotation)
        | Div -> Const_real (float_of_int i1 /. f2, annotation)
        | Mod -> Const_real (mod_float (float_of_int i1) f2, annotation)
        | Eq -> Const_bool (float_of_int i1 = f2, annotation)
        | Neq -> Const_bool (float_of_int i1 <> f2, annotation)
        | Lt -> Const_bool (float_of_int i1 < f2, annotation)
        | Gt -> Const_bool (float_of_int i1 > f2, annotation)
        | Leq -> Const_bool (float_of_int i1 = f2 && float_of_int i1 < f2, annotation)
        | Geq -> Const_bool (float_of_int i1 = f2 && float_of_int i1 > f2, annotation)
        | _ -> failwith "undefined")
      | _ -> failwith "undefined")
    | _ -> failwith "undefined")
  | _ -> failwith "undefined")



let rec simplify_expr expr =
  match expr with
  | Binop (op, e1, e2, annotation) -> (
      let simplified_e1 = simplify_expr e1 in
      match simplified_e1 with
      | Cst_i (i1, _) -> (
          let simplified_e2 = simplify_expr e2 in
          match simplified_e2 with
          | Cst_i (i2, _) -> (
              match op with
              | Add -> Cst_i (i1 + i2, annotation)
              | Sub -> Cst_i (i1 - i2, annotation)
              | Mul -> Cst_i (i1 * i2, annotation)
              | Div -> Cst_i (i1 / i2, annotation)
              | Mod -> Cst_i (i1 mod i2, annotation)
              | Eq -> Cst_b (i1 = i2, annotation)
              | Neq -> Cst_b (i1 <> i2, annotation)
              | Lt -> Cst_b (i1 < i2, annotation)
              | Gt -> Cst_b (i1 > i2, annotation)
              | Leq -> Cst_b (i1 = i2 && i1 < i2, annotation)
              | Geq -> Cst_b (i1 = i2 && i1 > i2, annotation)
              | _ -> failwith "undefined")
          | _ -> failwith "undefined")
      | Cst_f (f1, _) -> (
          let simplified_e2 = simplify_expr e2 in
          match simplified_e2 with
          | Cst_f (f2, _) -> (
              match op with
              | Add -> Cst_f (f1 +. f2, annotation)
              | Sub -> Cst_f (f1 -. f2, annotation)
              | Mul -> Cst_f (f1 *. f2, annotation)
              | Div -> Cst_f (f1 /. f2, annotation)
              | Mod -> Cst_f (mod_float f1 f2, annotation)
              | Eq -> Cst_b (f1 = f2, annotation)
              | Neq -> Cst_b (f1 <> f2, annotation)
              | Lt -> Cst_b (f1 < f2, annotation)
              | Gt -> Cst_b (f1 > f2, annotation)
              | Leq -> Cst_b (f1 = f2 && f1 < f2, annotation)
              | Geq -> Cst_b (f1 = f2 && f1 > f2, annotation)
              | _ -> failwith "undefined")
          | _ -> failwith "undefined")
      | Cst_b (b1, _) -> (
          let simplified_e2 = simplify_expr e2 in
          match simplified_e2 with
          | Cst_b (b2, _) -> (
              match op with
              | And -> Cst_b (b1 && b2, annotation)
              | Or -> Cst_b (b1 || b2, annotation)
              | Eq -> Cst_b (b1 = b2, annotation)
              | Neq -> Cst_b (b1 <> b2, annotation)
              | Lt -> Cst_b (b1 < b2, annotation)
              | Gt -> Cst_b (b1 > b2, annotation)
              | Leq -> Cst_b (b1 = b2 && b1 < b2, annotation)
              | Geq -> Cst_b (b1 = b2 && b1 > b2, annotation)
              | _ -> failwith "undefined")
          | _ -> failwith "undefined")
      | _ -> failwith "undefined")
  | _ -> expr

let rec simplify_instruction instruction =
  match instruction with
  | Affect (name, expr, annotation) ->
      let simplified_expr = simplify_expr expr in
      Affect (name, simplified_expr, annotation)
  | IfThenElse (test, i_then, i_else, annotation) -> (
      let simplified_expr = simplify_expr test in
      match simplified_expr with
      | Cst_b (true, _) -> simplify_instruction i_then
      | Cst_b (false, _) -> simplify_instruction i_else
      | _ ->
          IfThenElse
            ( simplified_expr,
              simplify_instruction i_then,
              simplify_instruction i_else,
              annotation ))
  | While (test, body, annotation) -> (
      let simplified_expr = simplify_expr test in
      match simplified_expr with
      | Cst_b (false, annotation) -> Block ([], annotation)
      | _ -> While (simplify_expr test, body, annotation))
  | Print_expr (expr, annotation) ->
      let simplified_expr = simplify_expr expr in
      Print_expr (simplified_expr, annotation)
  | _ -> instruction

let simplify_func_decl = function
  | Func_decl (typ, name, args, body, annotation) ->
      Func_decl (typ, name, args, simplify_instruction body, annotation)
