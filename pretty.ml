open Ast
open Printf
open Tast

let is_pretty = ref true

let fold_string f list = String.concat "" (List.map f list)
let fold_children f list parent_id = fold_string (fun item -> f item parent_id) list

let fold_hashtbl_children f hashtbl parent_id =
  fold_string (fun item -> f item parent_id) (Hashtbl.fold (fun k v acc -> (k, v) :: acc) hashtbl [])

let get_node_id, reset_node_id =
  let id = ref 0 in
  ( fun name -> ( incr id; sprintf "%s_%d" name !id ) ),
  fun () -> id := 0

let link_nodes id_from id_to = sprintf "%s -> %s\n" id_from id_to

let html_attribute balise attribute name =  sprintf "<%s %s>%s</%s>" balise attribute name balise
let html balise name = html_attribute balise "" name

let draw_node id =
  let draw_node_no_pretty id name fields children =
    if fields = [] then "" else sprintf "%s:%s\n" id (String.concat "" fields) in

  let draw_node_pretty id name fields children =
    let colspan = sprintf "colspan = '%d'" (max 1 (List.length children)) in
    let rowfield field = html "tr" (html_attribute "td" colspan field) in
    let rowchild child = html_attribute "td" (sprintf "port='%s'" child) child in
    let table = html_attribute "table" "border='0' cellborder='1' cellspacing='0' cellpadding='4'" (
        html "tr" (html_attribute "td" colspan (html "b" name)) ^
        ( if fields = [] then "" else fold_string rowfield fields ) ^
        ( if children = [] then "" else html "tr" (fold_string rowchild children) )
      )
    in sprintf "%s [label=<%s>]\n" id table in

  if !is_pretty then draw_node_pretty id else draw_node_no_pretty id

let create_node parent_id name fields children =
  let node_id = get_node_id name in
  let node_children = List.map (fun (child, _) -> child) children
  in draw_node node_id name fields node_children ^
     link_nodes parent_id node_id ^
     fold_string (fun (child, f) -> f (sprintf "%s:%s" node_id child)) children

let draw_root_ast f =
  reset_node_id ();
  let ast = draw_node "root" "*" [] [] ^ f "root" in

  if !is_pretty then
    "digraph ast {\n" ^
    "node [shape=plaintext];\n" ^
    ast ^ "}"
  else
    ast


let binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Beq -> "=="
  | Bne -> "!="
  | Blt -> "&lt;"
  | Ble -> "&le;"
  | Bgt -> "&gt;"
  | Bge -> "&ge;"
  | Band -> "&amp;&amp;"
  | Bor -> "||"

let unop = function
  | Uneg -> "-"
  | Unot -> "!"
  | Uamp -> "&amp;"
  | Ustar -> "*"

let incdec = function
  | Inc -> "++"
  | Dec -> "--"


let get_ast_constant constant parent_id =
  let create_node = create_node parent_id in
  match constant with
  | Cbool bool -> create_node "Cbool" [ sprintf "bool = \"%b\"" bool ] []
  | Cint int64 -> create_node "Cint" [ sprintf "int64 = \"%Ld\"" int64 ] []
  | Cstring string -> create_node "Cstring" [ sprintf "string = \"%s\"" string ] []

let rec get_ast_typ ptyp parent_id =
  let create_node = create_node parent_id in
  match ptyp with
  | PTident ident -> create_node "PTident" [ "ident.id = \"" ^ ident.id ^ "\"" ] []
  | PTptr ptyp -> create_node "PTptr" [] [ ("ptyp", get_ast_typ ptyp) ]

let rec get_ast_pexpr { pexpr_desc } parent_id =
  let create_node = create_node parent_id in
  match pexpr_desc with
  | PEskip -> create_node "PEskip" [] []

  | PEconstant constant -> create_node "PEconstant" [] [
      ("constant", get_ast_constant constant)
    ]

  | PEbinop (op, pexpr_left, pexpr_right) ->
    create_node "PEbinop" [ "binop = \"" ^ binop op ^ "\"" ] [
      ("pexpr_left", get_ast_pexpr pexpr_left);
      ("pexpr_right", get_ast_pexpr pexpr_right)
    ]

  | PEunop (op, pexpr) ->
    create_node "PEunop" [ "unop = \"" ^ unop op ^ "\"" ] [
      ("pexpr", get_ast_pexpr pexpr)
    ]

  | PEnil -> create_node "PEnil" [] []

  | PEcall (ident, pexprs) -> create_node "PEcall" [ "ident.id = \"" ^ ident.id ^ "\"" ] [
      ("pexpr_list", fold_children get_ast_pexpr pexprs)
    ]

  | PEident ident -> create_node "PEident" [ "ident.id = \"" ^ ident.id ^ "\"" ] []

  | PEdot (pexpr, ident) -> create_node "PEdot" [ "ident.id = \"" ^ ident.id ^ "\"" ] [
      ("pexpr", get_ast_pexpr pexpr)
    ]

  | PEassign (pexprs_left, pexprs_right) -> create_node "PEassign" [] [
      ("pexpr_list_left", fold_children get_ast_pexpr pexprs_left);
      ("pexpr_list_right", fold_children get_ast_pexpr pexprs_right)
    ]

  | PEvars (idents, ptyp, pexprs) ->
    let ident_list =
      String.concat ", " (List.map (fun { id } -> "{ id = \"" ^ id ^ "\" }") idents)
    in let ptyp_field = match ptyp with
        | Some ptyp -> [("ptyp", get_ast_typ ptyp)]
        | None -> []
    in let fields = ptyp_field @ [ ("pexpr_list", fold_children get_ast_pexpr pexprs) ]
    in create_node "PEvars" [ "idents = [" ^ ident_list ^ "]" ] fields

  | PEif (cond, if_block, else_block) -> create_node "PEif" [] [
      ("pexpr_cond", get_ast_pexpr cond);
      ("pexpr_if", get_ast_pexpr if_block);
      ("pexpr_else", get_ast_pexpr else_block)
    ]

  | PEreturn pexprs -> create_node "PEreturn" [] [
      ("pexpr_list", fold_children get_ast_pexpr pexprs)
    ]

  | PEblock pexprs -> create_node "PEblock" [] [
      ("pexpr_list", fold_children get_ast_pexpr pexprs)
    ]

  | PEfor (cond, for_block) -> create_node "PEfor" [] [
      ("pexpr_cond", get_ast_pexpr cond);
      ("pexpr_for", get_ast_pexpr for_block);
    ]

  | PEincdec (pexpr, op) ->
    create_node "PEincdec" [ "incdec = \"" ^ incdec op ^ "\"" ] [
      ("pexpr", get_ast_pexpr pexpr);
    ]

let get_ast_pparam (ident, ptyp) parent_id =
  create_node parent_id "pparam" [ "ident.id = \"" ^ ident.id ^ "\"" ] [
    ("typ", get_ast_typ ptyp)
  ]

let get_ast_pfield (ident, ptyp) parent_id =
  create_node parent_id "pfield" [ "ident.id = \"" ^ ident.id ^ "\"" ] [
    ("typ", get_ast_typ ptyp)
  ]

let get_ast_pdecl pdecl parent_id =
  let create_node = create_node parent_id in
  match pdecl with
  | PDfunction { pf_name; pf_params; pf_typ; pf_body } ->
    create_node "PDfunction" [ "pf_name.id = \"" ^ pf_name.id ^ "\"" ] [
      ("pf_params", fold_children get_ast_pparam pf_params);
      ("pf_typ", fold_children get_ast_typ pf_typ);
      ("pf_body", get_ast_pexpr pf_body)
    ]

  | PDstruct { ps_name; ps_fields } ->
    create_node "PDstruct" [ "ps_name.id = \"" ^ ps_name.id ^ "\"" ] [
      ("ps_fields", fold_children get_ast_pfield ps_fields)
    ]

let get_dot_ast (_, pdecls) flag_is_pretty =
  is_pretty := flag_is_pretty;
  draw_root_ast (fold_children get_ast_pdecl pdecls)



let rec get_tast_typ typ parent_id =
  let create_node = create_node parent_id in
  match typ with
  | Tint -> create_node "Tint" [] []
  | Tbool -> create_node "Tbool" [] []
  | Tstring -> create_node "Tstring" [] []
  | Tstruct {s_name; s_fields} -> create_node "Tstruct" [ "s_name = \"" ^ s_name ^ "\""] []
  | Tptr typ -> create_node "Tptr" [] [ ("typ", get_tast_typ typ) ]
  | Twild -> create_node "Twild" [] []
  | Tmany typ_list -> create_node "Tmany" [] [ ("typ_list", fold_children get_tast_typ typ_list) ]

and get_tast_field { f_name; f_typ; f_ofs } parent_id =
  create_node parent_id "sfield" [
    "f_name = \"" ^ f_name ^ "\"";
    "f_ofs = \"" ^ sprintf "%d" f_ofs ^ "\""
  ] [
    ("f_typ", get_tast_typ f_typ)
  ]

let get_tast_var { v_name; v_id; v_typ; v_used} parent_id =
  create_node parent_id "var" [
    "v_name = \"" ^ v_name ^ "\"";
    "v_id = \"" ^ sprintf "%d" v_id ^ "\"";
    "v_used = \"" ^ sprintf "%b" v_used ^ "\"";
  ] [
    ("v_typ", get_tast_typ v_typ)
  ]

let rec get_tast_expr { expr_desc } parent_id =
  let create_node = create_node parent_id in
  match expr_desc with
  | TEskip -> create_node "TEskip" [] []

  | TEconstant constant -> create_node "TEconstant" [] [
      ("constant", get_ast_constant constant)
    ]

  | TEbinop (op, expr_left, expr_right) ->
    create_node "TEbinop" [ "binop = \"" ^ binop op ^ "\"" ] [
      ("pexpr_left", get_tast_expr expr_left);
      ("pexpr_right", get_tast_expr expr_right)
    ]

  | TEunop (op, expr) ->
    create_node "TEunop" [ "unop = \"" ^ unop op ^ "\"" ] [
      ("pexpr", get_tast_expr expr)
    ]

  | TEnil -> create_node "TEnil" [] []

  | TEnew typ -> create_node "TEnew" [] [
      ("typ", get_tast_typ typ)
    ]

  | TEcall ({ fn_name; fn_params; fn_typ }, exprs) ->
    create_node "TEcall" ["fn_name = \"" ^ fn_name ^ "\""] [
      ("fn_params", fold_children get_tast_var fn_params);
      ("fn_typ", fold_children get_tast_typ fn_typ);
      ("expr_list", fold_children get_tast_expr exprs)
    ]

  | TEident var -> create_node "TEident" [] [
      ("var", get_tast_var var)
    ]

  | TEdot (expr, field) -> create_node "TEdot" [] [
      ("expr", get_tast_expr expr);
      ("field", get_tast_field field)
    ]

  | TEassign (exprs_left, exprs_right) -> create_node "TEassign" [] [
      ("expr_list_left", fold_children get_tast_expr exprs_left);
      ("expr_list_right", fold_children get_tast_expr exprs_right)
    ]

  | TEvars (vars, expr) -> create_node "TEvars" [] [
      ("var_list", fold_children get_tast_var vars);
      ("assign_list", fold_children get_tast_expr expr)
    ]

  | TEif (cond, if_block, else_block) -> create_node "TEif" [] [
      ("expr_cond", get_tast_expr cond);
      ("expr_if", get_tast_expr if_block);
      ("expr_else", get_tast_expr else_block)
    ]

  | TEreturn exprs -> create_node "TEreturn" [] [
      ("expr_list", fold_children get_tast_expr exprs)
    ]

  | TEblock exprs -> create_node "TEblock" [] [
      ("expr_list", fold_children get_tast_expr exprs)
    ]

  | TEfor (cond, for_block) -> create_node "TEfor" [] [
      ("expr_cond", get_tast_expr cond);
      ("expr_for", get_tast_expr for_block);
    ]

  | TEprint exprs -> create_node "TEprint" [] [
      ("expr_list", fold_children get_tast_expr exprs)
    ]

  | TEincdec (expr, op) ->
    create_node "TEincdec" [ "incdec = \"" ^ incdec op ^ "\"" ] [
      ("expr", get_tast_expr expr);
    ]

let get_ast_pparam (ident, ptyp) parent_id =
  create_node parent_id "pparam" [ "ident.id = \"" ^ ident.id ^ "\"" ] [
    ("typ", get_ast_typ ptyp)
  ]

let get_tast_tdecl tdecl parent_id =
  let create_node = create_node parent_id in
  match tdecl with
  | TDfunction ({ fn_name; fn_params; fn_typ }, expr) ->
    create_node "TDfunction" [ "fn_name = \"" ^ fn_name ^ "\"" ] [
      ("fn_params", fold_children get_tast_var fn_params);
      ("fn_typ", fold_children get_tast_typ fn_typ);
      ("expr", get_tast_expr expr)
    ]

  | TDstruct { s_name; s_fields } ->
    create_node "TDstruct" [ "s_name = \"" ^ s_name ^ "\"" ] [
      ("s_fields", fold_hashtbl_children (fun (_, field) -> get_tast_field field) s_fields)
    ]

let get_dot_tast tdecls flag_is_pretty =
  is_pretty := flag_is_pretty;
  draw_root_ast (fold_children get_tast_tdecl tdecls)

