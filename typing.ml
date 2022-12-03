
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

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | _ -> false
    (* TODO autres types *)

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = 0; v_depth = 0 }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env
  let exists env s = M.mem s env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) false then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

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
    let te1,rt1 = expr env e1 and te2,rt2 = expr env e2 in
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
    let e,rt = l_expr env e1 in
      TEunop (Uamp, e), Tptr e.expr_typ, false

  | PEunop (Ustar, e1) ->
      l_expr_desc env loc (PEunop (Ustar, e1))

  | PEunop (Uneg, e1) ->
      let e, rt = expr env e1 in
      if e.expr_typ = Tint then TEunop (Uneg, e), Tint, false
      else error_typ loc e.expr_typ Tint

  | PEunop (Unot, e1) ->
      let e, rt = expr env e1 in
      if e.expr_typ = Tbool then TEunop (Unot, e), Tbool, false
      else error_typ loc e.expr_typ Tbool

  | PEcall ({id = "fmt.Print"}, el) ->
      let lexpr, lrt = List.split (List.map (expr env) el) in
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
    if List.length f.fn_typ = 1 then
      let ty = List.hd f.fn_typ in
        TEcall (f, tel), ty, false
    else TEcall (f, tel), Tmany f.fn_typ, false

  | PEfor (e, b) ->
    let te,rte = expr env e and tb, rtb = expr env b in
      if te.expr_typ <> Tbool then error_typ loc te.expr_typ Tbool
      else TEfor (te,tb), tvoid, rtb

  | PEif (e1, e2, e3) ->
    let te1,rt1 = expr env e1 and te2, rt2 = expr env e2 and te3, rt3 = expr env e3 in
    if te1.expr_typ <> Tbool then error_typ loc te1.expr_typ Tbool;
      TEif (te1, te2, te3), tvoid, rt2 || rt3

  | PEnil ->
    TEnil, Tptr Twild, false

  | PEident {id=id} as e ->
    let te,typ,rt = l_expr_desc env loc e in
      te, typ, false

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
      end else error loc ("there isn't a struct called " ^ s.s_name)

  | PEassign (lvl, el) ->
    let nlvl = List.length lvl and nel = List.length el in
    if nlvl = nel then begin
      let rec f env l = function
        | [] -> [],[]
        | h2::t2 -> let h1 = List.hd l and t1 = List.tl l in
          let te1, rt1 = l_expr env h1 and te2, rt2 = expr env h2 in
          let laux1,laux2 = f env t1 t2 in
          (te1::laux1, te2::laux2)
      in let l1,l2 = f env lvl el in TEassign (l1,l2), tvoid, false
    end else TEassign ([], []), tvoid, false 
    (* TODO *)

  | PEreturn el ->
    let tel, _ = List.split (List.map (expr env) el) in
    let typl = List.map (fun e -> e.expr_typ) tel in
      if List.length typl = 1 then TEreturn tel, List.hd typl, true
      else TEreturn tel, Tmany typl, true

  | PEblock el ->
    let tel = block_eval env loc el in
      TEblock tel, tvoid, false

  | PEincdec (e, op) ->
    let te, rt = l_expr env e in
      if te.expr_typ = Tint then TEincdec (te,op), Tint, false
      else error_typ loc te.expr_typ Tint

  | PEvars (il, tl, pel) -> (* Si on déclare une variable pas dans un bloc, ce n'est pas défini par les règles de typage : erreur *)
    error loc "variable need to be declared in a block"

and l_expr env e =
  let e, ty, rt = l_expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty}, rt

and l_expr_desc env loc = function
  | PEident id ->
    (try let v = Env.find id.id env in
      TEident v, v.v_typ, false
    with Not_found -> error loc ("unbound variable " ^ id.id))

  | PEdot (e,id) as expd ->
    let _ = l_expr env e in
    let te,rt = expr env {pexpr_loc= loc; pexpr_desc= expd} in
      te.expr_desc, te.expr_typ, false

  | PEunop (Ustar, e) as expd ->
      if e.pexpr_desc = PEnil then error loc "*nil is undefined"
      else let te, rt = expr env {pexpr_loc= loc; pexpr_desc= expd} in
        te.expr_desc, te.expr_typ, false

  | _ -> error loc "this expression is not a l-value"

and block_eval env loc = function
  | [] -> []

  | {pexpr_desc= PEvars (il, Some t, []); pexpr_loc= _}::l ->
    
    let tt = type_type t in
    let varl = ref [] in
    let up_env = List.fold_left (fun e id -> let nenv,v = Env.var id.id id.loc tt e in varl := v::!varl; nenv) env il in
    let telaux = block_eval up_env loc l in
    {expr_desc= TEvars (List.rev !varl, []); expr_typ= tvoid}::telaux

  | {pexpr_desc= PEvars (il, Some t, el); pexpr_loc= _}::l ->

    if (List.length il <> List.length el) && (List.length el <> 1) then error loc "var exp1"

    else if List.length il = List.length el then
      (let vexpl = List.map (fun (id,e) ->
        if Env.exists env id.id then error loc ("the variable " ^ id.id ^ " already exists")
        else let te, rt = expr env e and tt = type_type t in
          if te.expr_typ <> tt then error loc ("the variable has a type " ^ (type_to_string te.expr_typ) ^ " but is expected to have type " ^ (type_to_string tt))
          else (id, te)) (List.combine il el) in
      let varl = ref [] in
      let _,tel = List.split vexpl in
      let up_env = List.fold_left (fun e (id, te) -> let nenv,v = (Env.var id.id id.loc te.expr_typ e) in varl := v::!varl; nenv) env vexpl in
      let telaux = block_eval up_env loc l in
      {expr_desc= TEvars (List.rev !varl, tel); expr_typ= tvoid}::telaux)

    else
      (let tef,tyl = match List.hd el with
        | {pexpr_desc= PEcall (idf, elf) as pef; pexpr_loc= loc} as ef ->

            let tef,rt = expr env ef in
        (match tef.expr_typ with
          | Tmany tyl ->
            if List.length l <> List.length il then error loc "the function return not enough values"
            else tef,tyl
          | _ -> error loc "this function return not enough values")

        | _ -> error loc "this expression need to be a function call"
      in

       let vexpl = List.map (fun (id,ty) ->
        if Env.exists env id.id then error loc ("the variable " ^ id.id ^ " already exists")
        else if (type_type t) <> ty then error loc ("this expression have a type " ^ (type_to_string ty) ^ " but is expected to have type " ^ (type_to_string (type_type t)))
        else (id, ty)) (List.combine il tyl)
      in
        let varl = ref [] in
        let up_env = List.fold_left (fun e (id, ty) -> let nenv,v = (Env.var id.id id.loc ty e) in varl := v::!varl; nenv) env vexpl in
        let telaux = block_eval up_env loc l in
        {expr_desc= TEvars (List.rev !varl, [tef]); expr_typ= tvoid}::telaux)
  | {pexpr_desc= PEvars (il, None, el); pexpr_loc= _}::l ->
    if (List.length il <> List.length el) && (List.length el <> 1) then error loc "var exp"

    else if List.length il = List.length el then
      (let vexpl = List.map (fun (id,e) ->
        if Env.exists env id.id then error loc ("the variable " ^ id.id ^ " already exists")
        else let te, rt = expr env e in (id, te)) (List.combine il el) in
      let varl = ref [] in
      let _,tel = List.split vexpl in
      let up_env = List.fold_left (fun e (id, te) -> let nenv,v = (Env.var id.id id.loc te.expr_typ e) in varl := v::!varl; nenv) env vexpl in
      let telaux = block_eval up_env loc l in
      {expr_desc= TEvars (List.rev !varl, tel); expr_typ= tvoid}::telaux)

    else 
      (let tef,tyl = match List.hd el with
      | {pexpr_desc= PEcall (idf, elf) as pef; pexpr_loc= loc} as ef ->

        let tef,rt = expr env ef in
        (match tef.expr_typ with
          | Tmany tyl ->
            if List.length l <> List.length il then error loc "the function return not enough values"
            else tef,tyl
          | _ -> error loc "this function return not enough values")

        | _ -> error loc "this expression need to be a function call"
      in

       let vexpl = List.map (fun (id,ty) ->
        if Env.exists env id.id then error loc ("the variable " ^ id.id ^ " already exists")
        else (id, ty)) (List.combine il tyl) in

      let varl = ref [] in
      let up_env = List.fold_left (fun e (id, ty) -> let nenv,v = (Env.var id.id id.loc ty e) in varl := v::!varl; nenv) env vexpl in
      let telaux = block_eval up_env loc l in
      {expr_desc= TEvars (List.rev !varl, [tef]); expr_typ= tvoid}::telaux)
  | {pexpr_desc= PEvars _; pexpr_loc= _}::l -> error loc "this var expression isn't correct"
  | e::l ->
      let te,rt = expr env e in
      let telaux = block_eval env loc l in
      te::telaux

let found_main = ref false

(* 1. declare structures *)
let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }} -> ignore (Struct_Env.void_struct id)
  | PDfunction _ -> ()

let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Twild -> assert false
  | Tmany l -> let sl = List.map sizeof l in List.fold_left (+) 0 sl
  | Tstruct s -> s.s_size

(* 2. declare functions and type fields *)
let var_of_pparam (id, typ) = new_var id.id id.loc (type_type typ)

let phase2 = function
  | PDfunction { pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } ->
      if (id = "main" && pl = []) then found_main := true;
      ignore (Fun_Env.func id (List.map var_of_pparam pl) (List.map type_type tyl))
  | PDstruct { ps_name = {id}; ps_fields = fl } ->
      let ht = Hashtbl.create (List.length fl) in
        List.iter (fun (fid, ftyp) -> let f = {f_name= fid.id; f_typ= type_type ftyp; f_ofs= 0} in Hashtbl.add ht fid.id f) fl; 
      let size = Hashtbl.fold (fun _ f i -> i + (sizeof f.f_typ)) ht 0 in
        ignore (Struct_Env.struc id ht size)

(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    let f = Fun_Env.find id in
    let env = List.fold_left Env.add Env.empty f.fn_params in
    let e, rt = expr env e in
      Printf.printf "%s\n" id;
      TDfunction (f, e)
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
