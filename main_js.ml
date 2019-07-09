open Cduce
open Ast
open Types_additions
open Checker
open IO
open Js_of_ocaml
let pr str =
  let output = Dom_html.getElementById "output" in
  output##.innerHTML :=
    Js.string ((Js.to_string output##.innerHTML) ^ str)


let prf fmt =
  Format.ksprintf pr fmt

let print_logs () =
  let treat (exprid, data)  =
    if data.visited = 0 && data.ignored > 0
    then
      prf "<span class='warning'>%s : %s</span>\n"
        (Position.string_of_pos data.position)
        "Expression is unreachable!"
  in
  Seq.iter treat (all_logs ()) ;
  clear_logs ()


let type_check_string str =
  let program = parse_program_string str in
  let test_def (tenv,idm,env) (name,parsed_expr) =
    begin try
        prf "%s" name;
        let id = unique_varid () in
        let annot_expr = parser_expr_to_annot_expr tenv idm parsed_expr in
        let typ = typeof env annot_expr in
        let idm = StrMap.add name id idm in
        let env = ExprMap.add ((), Var id) typ env in
        prf ":%s\n" (Cduce.string_of_type typ) ; print_logs () ; (idm, env)
      with Ill_typed (pos, str) ->
        prf "<span class='error'>Ill-typed: %s : %s</span>\n"
          (Position.string_of_pos pos)
          str; (idm,env)
      end
    in
    let treat_elem (tenv,idm,env) elem =
      match elem with
      | Definition d ->
        let (idm,env) = test_def (tenv,idm,env) d in
        (tenv,idm,env)
      | Atoms lst ->
        let tenv = List.fold_left define_atom tenv lst in
        let env = add_atoms_to_env env lst tenv in
        (tenv,idm,env)
      | Types lst ->
        let tenv = define_types tenv lst in
        (tenv,idm,env)
    in
    ignore (List.fold_left treat_elem (empty_tenv, empty_id_map, empty_env) program)

let _ =

  let button = Dom_html.getElementById "checkButton" in
  Dom_html.addEventListener button Dom_html.Event.click
    (Dom_html.handler (fun _ ->
         match Dom_html.getElementById_coerce "code" Dom_html.CoerceTo.textarea with
         None -> Js._true
         | Some textArea ->  let txt = textArea##.value in
           type_check_string (Js.to_string txt);  Js._true)
    ) Js._false
