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

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

type env = {
  exit_label: string;
  ofs_this: int;
  nb_locals: int ref; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f b =
  let l_b = new_label () and l_end = new_label () in
  f l_b ++
  movq (imm (1-b)) (reg rdi) ++ jmp l_end ++
  label l_b ++ movq (imm b) (reg rdi) ++ label l_end

let rec expr env e = match e.expr_desc with
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
    (* TODO code pour constante string *) assert false 
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
    (* TODO code pour negation ints *) assert false 
  | TEunop (Unot, e1) ->
    expr env e1 ++
    cmpq (imm 0) (reg rdi) ++
    sete (reg dil)
  | TEunop (Uamp, e1) ->
    (* TODO code pour & *) assert false 
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false 
  | TEprint el ->
    List.fold_left (fun c e ->
      let pr = match e.expr_typ with
        | Tint -> call "print_int"
        | Tbool -> call "print_bool"
        | _ -> nop
      in
      c ++ expr env e ++ pr) nop el
    (* TODO code pour Print *)
  | TEident x ->
    (* TODO code pour x *) assert false 
  | TEassign ([{expr_desc=TEident x}], [e1]) ->
    (* TODO code pour x := e *) assert false 
  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *) assert false 
  | TEassign (_, _) ->
     assert false
  | TEblock el ->
    List.fold_left (fun c e -> c ++ expr env e) nop el
     (* TODO code pour block *)
  | TEif (e1, e2, e3) ->
     (* TODO code pour if *) assert false
  | TEfor (e1, e2) ->
     (* TODO code pour for *) assert false
  | TEnew ty ->
     (* TODO code pour new S *) assert false
  | TEcall (f, el) ->
     (* TODO code pour appel fonction *) assert false
  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *) assert false
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn [] ->
    (* TODO code pour return e *) assert false
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *) assert false
  | TEreturn _ ->
     assert false
  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  let s = f.fn_name in
    label ("F_" ^ s) ++ expr empty_env e ++ ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let print_int =
  label "print_int" ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab "S_int") (reg rdi) ++
  xorq (reg rax) (reg rax) ++
  call "printf" ++
  ret

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


    
  

let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  (* TODO code fonctions *) let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++
      print_int ++
      print_bool
; (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      label "S_int" ++ string "%d" ++
      label "S_true" ++ string "true" ++
      label "S_false" ++ string "false" ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
