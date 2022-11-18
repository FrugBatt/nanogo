
(* arbres issus du parseur *)

type location = Lexing.position * Lexing.position

type ident = { loc : location; id : string }

type unop =
  | Uneg | Unot | Uamp | Ustar

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Beq | Bne | Blt | Ble | Bgt | Bge
  | Band | Bor (* && || *)

type constant =
  | Cbool of bool
  | Cint of int64
  | Cstring of string

type ptyp =
  | PTident of ident (* bool, int, struct id *)
  | PTptr   of ptyp

type incdec = Inc | Dec (* ++ -- *)

type pexpr =
  { pexpr_desc : pexpr_desc;
    pexpr_loc  : location; }

and pexpr_desc =
  | PEskip
  | PEconstant of constant
  | PEbinop of binop * pexpr * pexpr
  | PEunop of unop * pexpr
  | PEnil
  | PEcall of ident * pexpr list
  | PEident of ident
  | PEdot of pexpr * ident
  | PEassign of pexpr list * pexpr list
  | PEvars of ident list * ptyp option * pexpr list
  | PEif of pexpr * pexpr * pexpr
  | PEreturn of pexpr list
  | PEblock of pexpr list
  | PEfor of pexpr * pexpr
  | PEincdec of pexpr * incdec

and pparam = ident * ptyp

type pfunc = {
  pf_name   : ident;
  pf_params : pparam list;
  pf_typ    : ptyp list;
  pf_body   : pexpr;
}

type pfield = ident * ptyp

type pstruct = {
  ps_name   : ident;
  ps_fields : pfield list;
}

type pdecl =
  | PDfunction of pfunc
  | PDstruct   of pstruct

type import_fmt = bool

type pfile = import_fmt * pdecl list
