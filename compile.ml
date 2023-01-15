(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

open Format
open Ast
open Tast
open X86_64

exception Anomaly of string

let debug = ref false

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let rec insert_list x = function
  | [] -> []
  | [e] -> [e]
  | h::t -> h::x::(insert_list x t)

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

(* module Env = struct *)
(*   module M = Map.Make(String) *)
(*   type t = { *)
(*     exit_label: string; *)
(*     ofs_this: int; *)
(*     nb_locals: int; (* maximum *) *)
(*     next_local: int; (* 0, 1, ... *) *)
(*     vars_ofs: int M.t *)
(*   } *)
(*   let empty_env = {exit_label= ""; ofs_this= -1; nb_locals= 0; next_local= 0; vars_ofs= M.empty} *)
(*   let fun_env fn_name = {empty_env with exit_label= "E_" ^ fn_name} *)
(**)
(*   let get_ofs e v_name = M.find v_name e.vars_ofs *)
(**)
(*   let incr_ofs e = *)
(*     let vars = M.map (fun ofs -> ofs - 8) e.vars_ofs in *)
(*     {e with vars_ofs= vars } *)
(*   let add_var e v_name = *)
(*     let vars = M.map (fun ofs -> ofs - 8) e.vars_ofs in *)
(*     {e with vars_ofs= M.add v_name 0 vars; nb_locals= e.nb_locals + 1} *)
(**)
(* end *)

type env = {
  exit_label: string;
  ofs_this: int;
  mutable nb_locals: int; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}


let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = 0; next_local = 0 }

let fun_env f =
  { empty_env with exit_label = "E_" ^ f.fn_name; nb_locals= List.length f.fn_params}

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f b =
  let l_b = new_label () and l_end = new_label () in
  f l_b ++
  movq (imm (1-b)) (reg rdi) ++ jmp l_end ++
  label l_b ++ movq (imm b) (reg rdi) ++ label l_end

let default_value = function
  | Tstruct s ->
    movq (imm s.s_size) (reg rdi) ++
    call "allocz" ++
    movq (reg rax) (reg rdi)
  | _ -> xorq (reg rdi) (reg rdi)

let push_ret arg_s ret_s =
  let c = ref nop in
  for i = 1 to (ret_s / 8) do
    c := !c ++ pushq (ind rsp ~ofs:(-8 - arg_s))
  done;
  !c



let rec expr ?(copy=false) env e = match e.expr_desc with
  | TEskip ->
    nop
  | TEconstant (Cbool true) ->
    movq (imm 1) (reg rdi)
  | TEconstant (Cbool false) ->
    movq (imm 0) (reg rdi)
  | TEconstant (Cint x) ->
    movq (imm64 x) (reg rdi)
  | TEnil ->
    xorq (reg rdi) (reg rdi)
  | TEconstant (Cstring s) ->
    let slab = alloc_string s in
      movq (ilab slab) (reg rdi)
  | TEbinop (Band, e1, e2) ->
    let gen_as lab =
      expr env e1 ++
      cmpq (imm 0) (reg rdi) ++
      je lab ++
      expr env e2 ++
      cmpq (imm 0) (reg rdi) ++
      je lab
    in compile_bool gen_as 0
  | TEbinop (Bor, e1, e2) ->
    let gen_as lab =
      expr env e1 ++
      cmpq (imm 1) (reg rdi) ++
      je lab ++
      expr env e2 ++
      cmpq (imm 1) (reg rdi) ++
      je lab
    in compile_bool gen_as 1
  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
    let bool_gen lab = match op with
      | Blt -> jl lab
      | Ble -> jle lab
      | Bgt -> jg lab
      | Bge -> jge lab
      | _ -> nop (* impossible *)
    in
      expr env e1 ++
      movq (reg rdi) (reg rsi) ++
      expr env e2 ++
      cmpq (reg rdi) (reg rsi) ++
      compile_bool bool_gen 1
  | TEbinop (Badd | Bsub | Bmul | Bdiv | Bmod as op, e1, e2) ->
    let as_op = match op with
      | Badd -> addq (reg rax) (reg rdi)
      | Bsub -> subq (reg rax) (reg rdi)
      | Bmul -> imulq (reg rax) (reg rdi)
      | Bdiv -> cqto ++ idivq (reg rdi) ++ movq (reg rax) (reg rdi)
      | Bmod -> cqto ++ idivq (reg rdi) ++ movq (reg rdx) (reg rdi)
      | _ -> nop (* impossible *)
    in
      expr env e1 ++
      pushq (reg rdi) ++
      expr env e2 ++
      popq rax ++
      as_op
  | TEbinop (Beq | Bne as op, e1, e2) ->
    let eq_gen lab = match op with
      | Beq -> je lab
      | Bne -> jne lab
      | _ -> nop (* impossible *)
    in
      expr env e1 ++
      movq (reg rdi) (reg rsi) ++
      expr env e2 ++
      cmpq (reg rdi) (reg rsi) ++
      compile_bool eq_gen 1
  | TEunop (Uneg, e1) ->
      expr env e1 ++
      negq (reg rdi)
  | TEunop (Unot, e1) ->
    expr env e1 ++
    cmpq (imm 0) (reg rdi) ++
    sete (reg dil)
  | TEunop (Uamp, e1) ->
    l_expr env e1
  | TEunop (Ustar, e1) ->
    expr env e1 ++
    (match e.expr_typ with
      | Tstruct _ -> nop
      | _ -> movq (ind rdi) (reg rdi))
  | TEprint el ->
    let prints = List.map (fun e ->
      let pr = match e.expr_typ with
        | Tint -> call "print_int"
        | Tbool -> call "print_bool"
        | Tstring -> call "print_string"
        | Tptr _ -> call "print_ptr"
        | Tstruct s -> call ("print_struct_"^s.s_name)
        | _ -> nop
      in expr env e ++ pr) el in
    let pspace = call "print_space" in
    List.fold_left (fun c e -> c ++ e) nop (insert_list pspace prints)
  | TEident x -> (match x.v_typ with
    | Tstruct s when copy ->
      movq (imm s.s_size) (reg rdi) ++
      call "allocz" ++
      l_expr env e ++
      pushq (reg rbx) ++
      movq (reg rax) (reg rbx) ++
      movq (reg rax) (reg rsi) ++
      movq (imm s.s_size) (reg rdx) ++
      call "copy_struct" ++ (* copy_struct doesn't use %rbx *)
      movq (reg rbx) (reg rdi) ++
      popq rbx
    | _ -> movq (ind ~ofs:x.v_addr rbp) (reg rdi))
  | TEassign ([v], [e]) ->
    l_expr env v ++
    movq (reg rdi) (reg rsi) ++
    (* expr env e ~copy:true ++ *)
    expr env e ++
    (match e.expr_typ with
      | Tstruct s -> movq (imm s.s_size) (reg rdx) ++ call "copy_struct"
      | _ -> movq (reg rdi) (ind rsi))
  | TEassign (vl, [e]) ->
    expr env e ++
    (List.fold_left (fun c v ->
      c ++
      l_expr env v ++
      popq rbx ++
      (match v.expr_typ with
        | Tstruct s -> movq (reg rdi) (reg rsi) ++ movq (reg rbx) (reg rdi) ++ movq (imm s.s_size) (reg rdx) ++ call "copy_struct"
        | _ -> movq (reg rbx) (ind rdi)))) nop (List.rev vl)
  | TEassign (vl, el) ->
    let vel = List.combine vl el in
    List.fold_left (fun c (v,e) ->
      c ++
      l_expr env v ++
      movq (reg rdi) (reg rsi) ++
      (* expr env e ~copy:true ++ *)
      expr env e ++
      (match v.expr_typ with
        | Tstruct s -> movq (imm s.s_size) (reg rdx) ++ call "copy_struct"
        | _ -> movq (reg rdi) (ind rsi))) nop vel
  | TEblock el ->
    eval_block env el
  | TEif (e1, e2, e3) ->
    let lab_else = new_label () and lab_end = new_label () in
      expr env e1 ++
      testq (reg rdi) (reg rdi) ++
      jz lab_else ++
      expr env e2 ++
      jmp lab_end ++
    label lab_else ++
      expr env e3 ++
    label lab_end
  | TEfor (e1, e2) ->
    let lab_loop_cond = new_label () and lab_loop_end = new_label () in
    label lab_loop_cond ++
      expr env e1 ++
      testq (reg rdi) (reg rdi) ++
      jz lab_loop_end ++
      expr env e2 ++
      jmp lab_loop_cond ++
    label lab_loop_end
  | TEnew ty ->
    movq (imm (sizeof ty)) (reg rdi) ++
    call "allocz" ++
    movq (reg rax) (reg rdi)
  | TEcall (f, el) ->
    (* let arg_s = List.fold_left (fun s v -> s + (sizeof v.v_typ)) 0 f.fn_params in *)
    (* let ret_s = List.fold_left (fun s ty -> s + (sizeof ty)) 0 f.fn_typ in *)
    let arg_s = (List.length f.fn_params) * 8 in
    let ret_s = (List.length f.fn_typ) * 8 in
      (List.fold_left (fun c exp ->
        c ++
        expr env exp ~copy:true ++
        (match exp.expr_typ with
          | Tmany _ -> nop
          | _ -> pushq (reg rdi))) nop el) ++
      call ("F_"^f.fn_name) ++
      addq (imm (ret_s + arg_s)) (reg rsp) ++
      push_ret arg_s ret_s
  | TEdot (e1, f) ->
    expr env e1 ++
    (match f.f_typ with
      | Tstruct _ -> addq (imm f.f_ofs) (reg rdi)
      | _ -> movq (ind rdi ~ofs:f.f_ofs) (reg rdi))
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn [] ->
    jmp env.exit_label
  | TEreturn [e1] ->
    expr env e1 ++
    jmp env.exit_label
  | TEreturn el ->
    (List.fold_left (fun c exp ->
      c ++
      expr env exp ++
      pushq (reg rdi)) nop el) ++
    jmp env.exit_label
  | TEincdec (e1, op) ->
    let as_op = match op with
      | Inc -> incq (ind rdi)
      | Dec -> decq (ind rdi)
    in
      l_expr env e1 ++
      as_op

and l_expr env e = match e.expr_desc with
  | TEident x ->
    (match e.expr_typ with
      | Tstruct _ -> movq (ind rbp ~ofs:x.v_addr) (reg rdi)
      | _ -> movq (reg rbp) (reg rdi) ++ addq (imm x.v_addr) (reg rdi))
  | TEunop (Ustar, e) ->
    expr env e
  | TEdot (e,x) ->
    expr env e ++
    addq (imm x.f_ofs) (reg rdi)
  | _ -> assert false (* ce n'est pas une l-value *)

and eval_block env = function
  | [] -> nop
  | {expr_desc= TEvars (vl, el)}::t ->
    let id = ref ((-8) * (env.nb_locals + 1)) in
      List.iter (fun v -> v.v_addr <- !id; id := !id-8) vl;
      env.nb_locals <- env.nb_locals + (List.length vl);
      (if el = [] then List.fold_left (fun c v ->
        c ++
        default_value v.v_typ ++
        pushq (reg rdi)) nop vl
      else List.fold_left (fun c exp ->
        c ++
        expr env exp ~copy:true ++
        (match exp.expr_typ with
          | Tmany _ -> nop
          | _ -> pushq (reg rdi))) nop el) ++
      eval_block env t
  | h::t -> expr env h ++ eval_block env t

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  let s = f.fn_name in
  let env = fun_env f in
  let args_addr = ref ((List.length f.fn_params) * 8 + 8) in
  List.iter (fun v -> v.v_addr <- !args_addr; args_addr := !args_addr - 8) f.fn_params;
    label ("F_" ^ s) ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    expr env e ++
    label ("E_" ^ s) ++
    (* movq (ind rbp ~ofs:8) (reg r15) ++ *)
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    (if List.length f.fn_typ > 1 then
      popq r15 ++
      push_ret 16 ((List.length f.fn_typ)*8) ++
      pushq (reg r15)
    else nop) ++
    ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let print_bool =
  let l_false = new_label () in
    label "print_bool" ++
    xorq (reg rax) (reg rax) ++
    cmpq (imm 0) (reg rdi) ++
    je l_false ++
    movq (ilab "S_true") (reg rdi) ++
    call "printf" ++
    ret ++
    label l_false ++
    movq (ilab "S_false") (reg rdi) ++
    call "printf" ++
    ret

let print_int_or_nil =
  label "print_int_or_nil" ++
    testq (reg rdi) (reg rdi) ++
    jz "print_nil" ++
    movq (ind rdi) (reg rdi)

let print_int =
  label "print_int" ++
    movq (reg rdi) (reg rsi) ++
    movq (ilab "S_int") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    ret

let print_ptr =
  label "print_ptr" ++
    testq (reg rdi) (reg rdi) ++
    je "print_nil" ++
    movq (reg rdi) (reg rsi) ++
    movq (ilab "S_hexint") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    ret


let print_string =
  label "print_string" ++
    testq (reg rdi) (reg rdi) ++
    je "print_nil" ++
    movq (reg rdi) (reg rsi) ++
    movq (ilab "S_string") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    ret

let print_nil =
  label "print_nil" ++
    movq (ilab "S_nil") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    ret

let print_space =
  label "print_space" ++
    movq (ilab "S_space") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    ret

let allocz =
  let loop_lab = new_label () in
    label "allocz" ++
      pushq (reg rbx) ++
      movq (reg rdi) (reg rbx) ++
      call "malloc" ++
    label loop_lab ++
      decq (reg rbx) ++
      movb (imm 0) (ind rax ~index:rbx) ++
      testq (reg rbx) (reg rbx) ++
      jnz loop_lab ++
      popq rbx ++
      ret

(* rdi = src ; rsi = dest ; rdx = length *)
let copy_struct =
  let end_lab = new_label () in
    label "copy_struct" ++
      testq (reg rdx) (reg rdx) ++
      jz end_lab ++
      movq (ind rdi) (reg rcx) ++
      movq (reg rcx) (ind rsi) ++
      addq (imm 8) (reg rdi) ++
      addq (imm 8) (reg rsi) ++
      subq (imm 8) (reg rdx) ++
      jmp "copy_struct" ++
    label end_lab ++
      ret

let call_print_field f = match f.f_typ with
  | Tint -> call "print_int"
  | Tbool -> call "print_bool"
  | Tstring -> call "print_string"
  | Tptr _ -> call "print_ptr"
  | Tstruct s -> call ("print_struct_"^s.s_name)
  | _ -> nop


let print_struct s =
  label ("print_struct_"^s.s_name) ++
    pushq (reg r15) ++
    movq (reg rdi) (reg r15) ++
    movq (ilab "S_struct_open") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    (Hashtbl.fold (fun _ f c ->
      c ++
      movq (reg r15) (reg rdi) ++
      (* addq (imm f.f_ofs) (reg rdi) ++ *)
      (match f.f_typ with
        | Tstruct _ -> addq (imm f.f_ofs) (reg rdi)
        | _ -> movq (ind rdi ~ofs:f.f_ofs) (reg rdi)) ++
      xorq (reg rax) (reg rax) ++
      call_print_field f ++
      xorq (reg rax) (reg rax) ++
      call "print_space"
    )) s.s_fields nop ++
    movq (ilab "S_struct_close") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    popq r15 ++
    ret

let struct_print = ref []

let offset_decl = function
  | TDfunction _ -> ()
  | TDstruct s ->
    let ofs = ref 0 in
    Hashtbl.iter (fun _ f -> Printf.printf "ofs : %s.f_ofs = %d\n" f.f_name !ofs; f.f_ofs <- !ofs; ofs := !ofs + (sizeof f.f_typ)) s.s_fields;
    struct_print := (print_struct s)::!struct_print

let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *) List.iter offset_decl dl;
  (* TODO code fonctions *) let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++
      print_int_or_nil ++
      print_int ++
      print_ptr ++
      print_bool ++
      print_string ++
      print_nil ++
      print_space ++
      allocz ++
      copy_struct ++
      (List.fold_left (++) nop !struct_print)
    ;data =
      label "S_int" ++ string "%ld" ++
      label "S_hexint" ++ string "0x%x" ++
      label "S_true" ++ string "true" ++
      label "S_false" ++ string "false" ++
      label "S_string" ++ string "%s" ++
      label "S_nil" ++ string "<nil>" ++
      label "S_space" ++ string " " ++
      label "S_struct_open" ++ string "{ " ++
      label "S_struct_close" ++ string "}" ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
