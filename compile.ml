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

type env = {
  exit_label: string;
  mutable nb_locals: int; (* maximum *)
}


let empty_env =
  { exit_label = ""; nb_locals = 0 }

let fun_env f =
  { exit_label = "E_" ^ f.fn_name; nb_locals= 0}

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


(* La fonction expr génére le code assembly qui met dans le registre %rdi la valeur du TAST e *)
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
      | Bsub -> subq (reg rdi) (reg rax) ++ movq (reg rax) (reg rdi)
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
    let print_call exp = match exp.expr_typ with
      | Tint -> call "print_int"
      | Tbool -> call "print_bool"
      | Tstring -> call "print_string"
      | Tptr _ -> call "print_ptr"
      | Tstruct s -> call ("print_struct_"^s.s_name)
      | _ -> nop
    in 
    let rec gen_as = function
      | [] -> nop
      | [e1] -> expr env e1 ++ print_call e1
      | e1::e2::t ->
        if e1.expr_typ = Tstring || e2.expr_typ = Tstring then expr env e1 ++ print_call e1 ++ gen_as (e2::t)
        else expr env e1 ++ print_call e1 ++ call "print_space" ++ gen_as (e2::t)
    in
    gen_as el
      
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
    pushq (reg rdi) ++
    expr env e ++
    popq rsi ++
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
    List.fold_left (fun c e ->
      c ++
      expr env e ++
      pushq (reg rdi)) nop (List.rev el) ++
    List.fold_left (fun c v ->
      c ++
      l_expr env v ++
      movq (reg rdi) (reg rsi) ++
      popq (rdi) ++
      (match v.expr_typ with
        | Tstruct s -> movq (imm s.s_size) (reg rdx) ++ call "copy_struct"
        | _ -> movq (reg rdi) (ind rsi))) nop vl

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
    let arg_s = (List.length f.fn_params) * 8 in
    let ret_s = (List.length f.fn_typ) * 8 in
      (List.fold_left (fun c exp ->
        c ++
        expr env exp ~copy:true ++
        (match exp.expr_typ with
          | Tmany _ -> nop
          | _ -> pushq (reg rdi))) nop el) ++
      call ("F_"^f.fn_name) ++
      (match e.expr_typ with
        | Tmany _ -> addq (imm (ret_s + arg_s)) (reg rsp) ++ push_ret arg_s ret_s
        | _ -> addq (imm arg_s) (reg rsp))

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

  (* la fonction l_expr génére le code assembly qui met dans le registre %rdi l'adresse de la l-value e *)
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

  (* la fonction eval_block permet de générer les codes assembly récursivement d'un bloc et de gérer les déclarations de variables *)
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

(* la fonction function_ retourne le code assembly qui implémente la fonction f *)
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

(* la fonction decl retourne le code assembly correspondant à la déclaration d'une fonction ou d'une structure *)
let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

(* Fonctions assembly permettant l'affichage selon les différents types *)
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


(* Schéma de fonction assembly permettant l'affichage d'une structure s *)
let print_struct s =
  label ("print_struct_"^s.s_name) ++
    pushq (reg r15) ++
    movq (reg rdi) (reg r15) ++
    movq (ilab "S_struct_open") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    (let print_calls = List.map (fun name ->
      let f = Hashtbl.find s.s_fields name in
        movq (reg r15) (reg rdi) ++
        (match f.f_typ with
          | Tstruct _ -> addq (imm f.f_ofs) (reg rdi)
          | _ -> movq (ind rdi ~ofs:f.f_ofs) (reg rdi)) ++
        xorq (reg rax) (reg rax) ++
        call_print_field f 
    ) s.s_name_order in
    let code = insert_list (call "print_space") print_calls in
    List.fold_left (++) nop code) ++
    movq (ilab "S_struct_close") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    popq r15 ++
    ret

(* liste avec les différentes fonctions d'affichage *)
let struct_print = ref []

(* calcul des offset des fields d'une structure *)
let offset_decl = function
  | TDfunction _ -> ()
  | TDstruct s ->
    let ofs = ref 0 in
    Hashtbl.iter (fun _ f -> f.f_ofs <- !ofs; ofs := !ofs + (sizeof f.f_typ)) s.s_fields;
    struct_print := (print_struct s)::!struct_print

let file ?debug:(b=false) dl =
  debug := b;
  List.iter offset_decl dl;
  let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++
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
      label "S_struct_open" ++ string "{" ++
      label "S_struct_close" ++ string "}" ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
