
(* arbres issus du typeur *)

type unop = Ast.unop

type binop = Ast.binop

type constant = Ast.constant

type incdec = Ast.incdec

type function_ = {
    fn_name: string;
  fn_params: var list;
     fn_typ: typ list;
}

and field = {
         f_name: string;
          f_typ: typ;
  mutable f_ofs: int; (* relatif à l'adresse de l'objet *)
}

and structure = {
          s_name: string;
        s_fields: (string, field) Hashtbl.t;
  mutable s_size: int; (* taille calculee en octets *)
}

and typ =
  | Tint | Tbool | Tstring
  | Tstruct of structure
  | Tptr of typ
  | Twild (* type wildcard, tout type *)
  | Tmany of typ list (* 0 pour type retour instructions et >=2 pour retour functions *)
  (* TODO autres types pour l'analyse semantique, si besoin *)

and var = {
          v_name: string;
            v_id: int;  (* unique *)
           v_loc: Ast.location;
           v_typ: typ;
         v_depth: int;  (* index de portee *)
  mutable v_used: bool;
  mutable v_addr: int;  (* adresse relative au pointer de frame (rbp) *)
  (* TODO autres informations pour la production de code, si besoin *)
}

and expr =
  { expr_desc: expr_desc;
    expr_typ : typ; }

and expr_desc =
  | TEskip
  | TEconstant of constant
  | TEbinop of binop * expr * expr
  | TEunop of unop * expr
  | TEnil
  | TEnew of typ
  | TEcall of function_ * expr list
  | TEident of var
  | TEdot of expr * field
  | TEassign of expr list * expr list
  | TEvars of var list * expr list
  | TEif of expr * expr * expr
  | TEreturn of expr list
  | TEblock of expr list
  | TEfor of expr * expr
  | TEprint of expr list
  | TEincdec of expr * incdec

type tdecl =
  | TDfunction of function_ * expr
  | TDstruct of structure

type tfile = tdecl list
