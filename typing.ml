
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string
exception Anomaly of string

let error loc e = raise (Error (loc, e))

let rec type_to_string = function
  | Tint -> "int"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tstruct s -> "struct " ^ s.s_name
  | Tptr t -> "*"^(type_to_string t)
  | Twild -> "wild"
  | Tmany tl ->
    let sl = List.map type_to_string tl in
    "[" ^ (String.concat ";" sl) ^ "]"

let error_typed loc t1 t2 prefix = 
  let s1 = type_to_string t1 and s2 = type_to_string t2 in
  error loc (prefix ^ " expression has a type " ^ s1 ^ " but is expected to have type " ^ s2)

let error_typ loc t1 t2 = error_typed loc t1 t2 "this"

(* TODO environnement pour les types structure *)
module Struct_Env = struct
  let struct_env = Hashtbl.create 5

  let find s_name = Hashtbl.find struct_env s_name
  let add s = Hashtbl.add struct_env s.s_name s
  let exists s_name = Hashtbl.mem struct_env s_name

  let struc x fields size =
    let s = {s_name= x; s_fields= fields; s_size= size} in
      add s, s
  let void_struct x = struc x (Hashtbl.create 0) 0
end

(* TODO environnement pour les fonctions *)
module Fun_Env = struct
  let func_env = Hashtbl.create 5

  let find fn_name = Hashtbl.find func_env fn_name
  let add f = Hashtbl.add func_env f.fn_name f
  let exists fn_name = Hashtbl.mem func_env fn_name

  let func x params typ =
    let f = {fn_name= x; fn_params= params; fn_typ= typ} in
    add f, f
end

let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | PTident i ->
      let s = Struct_Env.find i.id in
        Tstruct s

let rec valid_type loc = function
  | PTident { id = "int" } -> ()
  | PTident { id = "bool" } -> ()
  | PTident { id = "string" } -> ()
  | PTptr ty -> valid_type loc ty
  | PTident i ->
      if Struct_Env.exists i.id then ()
      else error loc ("struct " ^ i.id ^ " is undefined")

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | _ -> false
    (* TODO autres types *)

let rec typ_of_list = function
  | [] -> Tmany []
  | [t] -> t
  | (Tmany l)::t -> typ_of_list (l@t)
  | h::t -> (match typ_of_list t with
    | Tmany [] -> h
    | Tmany l -> Tmany (h::l)
    | ty -> Tmany [h;ty])

(* let typ_of_list = function *)
(*   | [] -> Tmany [] *)
(*   | [t] -> t *)
(*   | l -> Tmany l *)

let rec list_of_typ = function
  | [] -> []
  | (Tmany l)::t -> l@(list_of_typ t)
  | h::t -> h::(list_of_typ t)
(* let list_of_typ = function *)
(*   | [] -> [] *)
(*   | [ty] -> [ty] *)
(*   | (Tmany l)::_ -> l *)
(*   | l -> l *)

let rec params_length = function
  | [] -> 0
  | {expr_desc= TEcall (f,_)}::t -> (List.length f.fn_typ) + (params_length t)
  | {expr_desc= TEreturn l}::t -> (params_length l) + (params_length t)
  | h::t -> 1 + (params_length t)

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref (-1) in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = 0; v_depth = 0 }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t

  let find = M.find
  let add env v = M.add v.v_name v env
  let exists env s = M.mem s env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && not v.v_used then error v.v_loc ("unused variable " ^ v.v_name) in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  let empty = 
    let map = M.empty in
    fst (var "_" dummy_loc ~used:true Twild map)

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let expr_id = function
  | {expr_desc= TEident v} -> v.v_name
  | {expr_desc= TEdot (_,v)} -> v.f_name
  | _ -> "no id"

let return_typ = ref tvoid

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

and expr_desc env loc = function
  | PEskip ->
      TEskip, tvoid, false

  | PEconstant c ->
      let ty = match c with
        | Cbool _ -> Tbool
        | Cint _ -> Tint
        | Cstring _ -> Tstring
      in TEconstant c, ty, false

  | PEbinop (op, e1, e2) ->
    let te1,_ = expr env e1 and te2,_ = expr env e2 in
    let ty = match op with
      | Beq | Bne ->
        if not (eq_type te1.expr_typ te2.expr_typ) then error loc "both expressions don't have the same type" 
        else if (te1.expr_desc = TEnil && te2.expr_desc = TEnil) then error loc "both expressions can't be nil"
        else Tbool
      | Blt | Ble | Bgt | Bge ->
          if te1.expr_typ <> Tint then error_typed e1.pexpr_loc te1.expr_typ Tint "left"
          else if te2.expr_typ <> Tint then error_typed e2.pexpr_loc te2.expr_typ Tint "right"
          else Tbool
      | Badd | Bsub | Bmul | Bdiv | Bmod ->
          if te1.expr_typ <> Tint then error_typed e1.pexpr_loc te1.expr_typ Tint "left"
          else if te2.expr_typ <> Tint then error_typed e2.pexpr_loc te2.expr_typ Tint "right"
          else Tint
      | Band | Bor ->
          if te1.expr_typ <> Tbool then error_typed e1.pexpr_loc te1.expr_typ Tbool "left"
          else if te2.expr_typ <> Tbool then error_typed e2.pexpr_loc te2.expr_typ Tbool "right"
          else Tbool
    in TEbinop (op, te1, te2), ty, false

  | PEunop (Uamp, e1) ->
    let e,_ = l_expr env e1 in
      TEunop (Uamp, e), Tptr e.expr_typ, false

  | PEunop (Ustar, e1) as e ->
      l_expr_desc env loc e

  | PEunop (Uneg, e1) ->
      let e, _ = expr env e1 in
      if e.expr_typ = Tint then TEunop (Uneg, e), Tint, false
      else error_typ loc e.expr_typ Tint

  | PEunop (Unot, e1) ->
      let e, _ = expr env e1 in
      if e.expr_typ = Tbool then TEunop (Unot, e), Tbool, false
      else error_typ loc e.expr_typ Tbool

  | PEcall ({id = "fmt.Print"}, el) ->
      let lexpr,_ = List.split (List.map (expr env) el) in
        fmt_used := true;
        TEprint lexpr, tvoid, false

  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
    let ty = match id with
      | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
      | s ->
        if Struct_Env.exists s then Tstruct (Struct_Env.find s)
        else error loc ("no such type " ^ id)
    in
      TEnew ty, Tptr ty, false

  | PEcall ({id="new"}, _) ->
      error loc "new expects a type"

  | PEcall (id, el) ->
    let f = Fun_Env.find id.id in
    let tel,_ = List.split (List.map (fun x -> expr env x) el) in 
      if (List.length f.fn_params) <> (params_length tel) then error loc "function arity doesn't match"
      else TEcall (f,tel), typ_of_list f.fn_typ, false

  | PEfor (e, b) ->
    let te,rte = expr env e and tb, rtb = expr env b in
      if te.expr_typ <> Tbool then error_typ loc te.expr_typ Tbool
      else TEfor (te,tb), tvoid, rtb

  | PEif (e1, e2, e3) ->
    let te1,_ = expr env e1 and te2, rt2 = expr env e2 and te3, rt3 = expr env e3 in
    if te1.expr_typ <> Tbool then error_typ loc te1.expr_typ Tbool;
      TEif (te1, te2, te3), tvoid, rt2 && rt3

  | PEnil ->
    TEnil, Tptr Twild, false

  | PEident {id=id} as e ->
    l_expr_desc env loc e

  | PEdot (e, id) ->
    let te, rt = expr env e in 
    let s = match te.expr_typ with
      | Tstruct s -> s
      | Tptr Twild -> error loc "this expression has a type *wild but is expected to have type struct or *struct"
      | Tptr (Tstruct s) -> s
      | t -> error loc ("this expression has a type " ^ (type_to_string t) ^ " but is expected to have type struct or *struct")
    in
      if Struct_Env.exists s.s_name then begin
        if Hashtbl.mem s.s_fields id.id then
          let f = Hashtbl.find s.s_fields id.id in
            TEdot (te, f), f.f_typ, false
        else error loc ("the struct " ^ s.s_name ^ " haven't a " ^ id.id ^ " field")
      end else error loc ("the struct " ^ s.s_name ^ " is not defined")

  | PEassign (lvl, el) ->
    let tel = List.map (fun e -> fst (expr env e)) el in
    let nlvl = List.length lvl and nel = params_length tel in
    if nlvl <> nel then error loc "arity doesn't match to assign each variable"
    else
      let tlvl = List.map (fun e -> fst (l_expr env ~underscore:true e)) lvl in
      let tyl = list_of_typ (List.map (fun e -> e.expr_typ) tel) in
      List.iter (fun (e1, ty) -> if e1.expr_typ <> ty then error loc ("the variable " ^ (expr_id e1) ^ " has a type " ^ (type_to_string e1.expr_typ) ^ " but is expected to have type " ^ (type_to_string ty))) (List.combine tlvl tyl);
        TEassign (tlvl,tel), tvoid, false

  | PEreturn el ->
    let tel, _ = List.split (List.map (expr env) el) in
    let ty = typ_of_list (List.map (fun e -> e.expr_typ) tel) in
      if ty = !return_typ then TEreturn tel, tvoid, true
      else error loc ("return value must be a " ^ (type_to_string !return_typ))

  | PEblock el ->
    let tel,rt = block_eval env el in
      TEblock tel, tvoid, rt

  | PEincdec (e, op) ->
    let te, rt = l_expr env e in
      if te.expr_typ = Tint then TEincdec (te,op), Tint, false
      else error_typ loc te.expr_typ Tint

  | PEvars (il, tl, pel) -> (* Si on déclare une variable pas dans un bloc, ce n'est pas défini par les règles de typage : erreur *)
    error loc "variable need to be declared in a block"

and l_expr env ?underscore e =
  let e, ty, rt = l_expr_desc env ?underscore e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty}, rt

and l_expr_desc env ?(underscore=false) loc = function
  | PEident id ->
    if id.id = "_" && not underscore then error loc ("variable _ can be used only in an assign expression")
    else
      (try let v = Env.find id.id env in
        v.v_used <- true;
        TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id.id))

  | PEdot (e,id) as expd ->
    let _ = l_expr env e in
    let te,_ = expr env {pexpr_loc= loc; pexpr_desc= expd} in
      te.expr_desc, te.expr_typ, false

  | PEunop (Ustar, e) as expd ->
      if e.pexpr_desc = PEnil then error loc "*nil is undefined"
      else let te,_ = expr env {pexpr_loc= loc; pexpr_desc= expd} in
        te.expr_desc, te.expr_typ, false

  | _ -> error loc "this expression is not a l-value"


and block_eval env = function
  | [] -> [],false
  
  | {pexpr_desc= PEvars (il, None, el); pexpr_loc= loc}::l ->
    let tel = List.map (fun exp -> fst (expr env exp)) el in
    let nvar = List.length il and nexp = params_length tel in
    if nvar = nexp then
      let tyl = list_of_typ (List.map (fun exp -> exp.expr_typ) tel) in
      (* Printf.printf "1. %d, 2. %d, 3. %d\n" (nexp) (List.length tel) (List.length tyl); *)
      let varl = ref [] in
      let up_env = List.fold_left (fun e (id, ty) -> let nenv,v = Env.var id.id id.loc ty e in varl := v::!varl; nenv) env (List.combine il tyl) in
      let telaux,rtaux = block_eval up_env l in
        {expr_desc= TEvars (List.rev !varl, tel); expr_typ= tvoid}::telaux, rtaux
    else error loc "this expression arity doesn't match"

  | {pexpr_desc= PEvars (il, Some t, [])}::l ->
    let ty = type_type t in
    let varl = ref [] in
    let up_env = List.fold_left (fun e id -> let nenv,v = Env.var id.id id.loc ty e in varl := v::!varl; nenv) env il in
    let telaux,rtaux = block_eval up_env l in
      {expr_desc= TEvars (List.rev !varl, []); expr_typ= tvoid}::telaux, rtaux

  | {pexpr_desc= PEvars (il, Some t, el); pexpr_loc= loc}::l ->
    let tel = List.map (fun exp -> fst (expr env exp)) el in
    let nvar = List.length il and nexp = params_length tel in
    if nvar = nexp then
      let ty = type_type t in
      let tyl = list_of_typ (List.map (fun exp -> exp.expr_typ) tel) in
      (* Printf.printf "1. %d, 2. %d, 3. %d\n" (nexp) (List.length tel) (List.length tyl); *)
      let varl = ref [] in
      let up_env = List.fold_left (fun e (id,vtyp) ->
        if ty = vtyp then begin
          let nenv, v = Env.var id.id id.loc ty e in
            varl := v::!varl;
            nenv
        end else error id.loc ("the variable " ^ id.id ^ " has a type " ^ (type_to_string vtyp) ^ " but is assigned a type " ^ (type_to_string ty))) env (List.combine il tyl) in
      let telaux,rtaux = block_eval up_env l in
        {expr_desc= TEvars (List.rev !varl, tel); expr_typ= tvoid}::telaux, rtaux
    else error loc "this expression arity doesn't match"

    | e::l ->
      let te,rt = expr env e in
      let telaux,rtaux = block_eval env l in
        te::telaux, rt || rtaux


let found_main = ref false

(* 1. declare structures *)
let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }} ->
    if Struct_Env.exists id then error loc ("struct " ^ id ^ " already defined")
    else ignore (Struct_Env.void_struct id)
  | PDfunction _ -> ()

let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Twild -> assert false
  | Tmany l -> let sl = List.map sizeof l in List.fold_left (+) 0 sl
  | Tstruct s -> s.s_size

(* 2. declare functions and type fields *)
let var_of_pparam (id, typ) = new_var id.id id.loc (type_type typ)
let rec unicity l =
  let rec aux uni = function
    | [] -> true
    | h::t ->
      if List.mem h uni then false
      else aux (h::uni) t
  in aux [] l

let phase2 = function
  | PDfunction { pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } ->
      if (id = "main" && pl = []) then found_main := true;
      if Fun_Env.exists id then error loc ("function " ^ id ^ " already defined")
      else
        List.iter (valid_type loc) tyl; (* Vérification des types de retour *)
        List.iter (fun (_,pty) -> valid_type loc pty) pl; (* Vérification des types des arguments *)
        let varl = List.map var_of_pparam pl in
        let varlnames = List.map (fun v -> v.v_name) varl in
        if unicity varlnames then
          ignore (Fun_Env.func id varl (List.map type_type tyl))
        else
          error loc ("function " ^ id ^ " args are not distincts")
  | PDstruct { ps_name = {id= id; loc= loc}; ps_fields = fl } ->
      let ht = Hashtbl.create (List.length fl) in
      let fnames = ref [] in
        List.iter (fun (fid, ftyp) -> 
          valid_type fid.loc ftyp;
          if List.mem fid.id !fnames then error fid.loc ("field " ^ fid.id ^ " already defined");
          fnames := fid.id :: !fnames;
          let f = {f_name= fid.id; f_typ= type_type ftyp; f_ofs= 0} in Hashtbl.add ht fid.id f) fl; 
        let size = Hashtbl.fold (fun _ f i -> i + (sizeof f.f_typ)) ht 0 in
          ignore (Struct_Env.struc id ht size)

(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    let f = Fun_Env.find id in
    let env = List.fold_left Env.add Env.empty f.fn_params in
      return_typ := (typ_of_list f.fn_typ);
      let e, rt = expr env e in
        if !return_typ <> tvoid && not rt then error loc ("the function " ^ f.fn_name ^ " have not a return expression in every case")
        else TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
    let s = Struct_Env.find id in
     TDstruct s

let file ~debug:b (imp, dl) =
  debug := b;
  (* fmt_imported := imp; *)
  List.iter phase1 dl;
  List.iter phase2 dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
