
/* Analyseur syntaxique pour Petit Go */

%{
  open Ast
  let mk_expr loc d = { pexpr_desc = d; pexpr_loc = loc }
  exception Error 
%}

%token <Ast.constant> CST
%token <string> IDENT
%token <string> STRING
%token EOF
%token PACKAGE IMPORT
%token FUNC TYPE STRUCT
%token FOR IF ELSE RETURN
%token VAR NIL
%token LEFTPAR RIGHTPAR LEFTBRACE RIGHTBRACE
%token SEMICOLON COMMA DOT AMP
%token COLONEQ EQ PLUSPLUS MINUSMINUS
%token VERTICALBARVERTICALBAR AMPERSANDAMPERSAND
%token <Ast.binop> COMP
%token PLUS MINUS STAR SLASH PERCENT
%token BANG

/* Définitions des priorités et associativités des tokens */

%left VERTICALBARVERTICALBAR
%left AMPERSANDAMPERSAND
%left COMP
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc USTAR UMINUS BANG AMP
%nonassoc DOT

/* Point d'entrée de la grammaire */
%start file

  /* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.pfile> file

%%

file:
  PACKAGE ident_main SEMICOLON
  imp = boption(import_fmt)
  dl = list(decl) EOF
  { imp, dl }
;

ident_main:
  id = ident { if id.id <> "main" then raise Error }
;

import_fmt:
| IMPORT s=STRING SEMICOLON { if s <> "fmt" then raise Error }
;

decl:
| FUNC id = ident; LEFTPAR pl = loption(parameters) RIGHTPAR;
  ty = loption(return_type); b = block
  SEMICOLON
  { PDfunction { pf_name = id;
                 pf_params = List.flatten pl;
                 pf_typ = ty;
                 pf_body = b } }
| TYPE id = ident STRUCT LEFTBRACE; fl=loption(fields); RIGHTBRACE SEMICOLON
  { PDstruct { ps_name = id; ps_fields = List.flatten fl; } }
;

fields:
| x = ids_and_type; SEMICOLON?             { [x]     }
| x = ids_and_type; SEMICOLON; xl = fields { x :: xl }

parameters:
| x = ids_and_type; COMMA ?                { [x]     }
| x = ids_and_type; COMMA; xl = parameters { x :: xl }
;

ids_and_type:
| ids = separated_nonempty_list(COMMA, ident) ty = type_expr
   { List.map (fun x -> x, ty) ids }
;

return_type:
| ty = type_expr                               { [ty] }
| LEFTPAR tyl = return_types RIGHTPAR { tyl }
;

return_types:
| ty = type_expr; COMMA?                    { [ty] }
| ty = type_expr; COMMA; tyl = return_types { ty :: tyl }
;

type_expr:
| id = ident
   { PTident id }
| STAR ty=type_expr
   { PTptr ty }
;

block:
| LEFTBRACE RIGHTBRACE
  { { pexpr_desc = PEblock []; pexpr_loc = $startpos, $endpos } }
| LEFTBRACE l=statements RIGHTBRACE
  { { pexpr_desc = PEblock l; pexpr_loc = $startpos, $endpos } }
;

statements:
| s=stmt                        { [s]    }
| s=stmt SEMICOLON              { [s]    }
| s=stmt SEMICOLON l=statements { s :: l }
|        SEMICOLON l=statements { l      }
|        SEMICOLON              { []     }
;

stmt:
| s=simple_stmt
    { s }
| b=block
    { b }
| s=if_stmt
    { s }
| d = stmt_desc
    { { pexpr_desc = d; pexpr_loc = $startpos, $endpos } }
;

stmt_desc:
| VAR ids=separated_nonempty_list(COMMA, ident)
      ty=option(type_expr) i=loption(init)
  { PEvars (ids, ty, i) }
| RETURN el = separated_list(COMMA, expr)
  { PEreturn el }
| FOR b = block
  { let loc = $startpos, $endpos in
    let etrue = mk_expr loc (PEconstant (Cbool true)) in
    PEfor (etrue, b) }
| FOR e1 = expr b = block
  { PEfor (e1, b) }
| FOR s1 = opt_simple_stmt SEMICOLON e2 = expr; SEMICOLON
      s3 = opt_simple_stmt b = block
  { let loc = $startpos, $endpos in
    let b = mk_expr loc (PEblock [b; s3]) in
    PEblock [s1; mk_expr loc (PEfor (e2, b))] }
;

if_stmt:
| d = if_stmt_desc
    { { pexpr_desc = d; pexpr_loc = $startpos, $endpos } }
;

if_stmt_desc:
| IF e = expr  s = block
  { PEif (e, s, { pexpr_desc = PEskip; pexpr_loc = $startpos, $endpos }) }
| IF e = expr s1 = block ELSE s2 = if_stmt
  { PEif (e, s1, s2) }
| IF e = expr s1 = block ELSE s2 = block
  { PEif (e, s1, s2) }

init:
| EQ el=exprs { el }
;

opt_simple_stmt:
| /* epsilon */   { { pexpr_desc = PEskip; pexpr_loc = $startpos, $endpos } }
| s = simple_stmt { s }
;

simple_stmt:
| e = expr
  { e }
| d = simple_stmt_desc
  { { pexpr_desc = d; pexpr_loc = $startpos, $endpos } }

simple_stmt_desc:
| lvl = exprs EQ el=exprs
  { PEassign (lvl, el) }
| lvl = exprs COLONEQ el=exprs
  { let var = function {pexpr_desc=PEident id} -> id | _ -> raise Error in
    PEvars (List.map var lvl, None, el) }
| e = expr i = incdec
  { PEincdec (e, i) }
;

incdec:
| PLUSPLUS   { Inc }
| MINUSMINUS { Dec }
;

exprs:
| el = separated_nonempty_list(COMMA, expr) { el }
;

expr:
| d = expr_desc
  { { pexpr_desc = d; pexpr_loc = $startpos, $endpos } }
;

expr_desc:
| c = CST
    { PEconstant c }
| s = STRING
    { PEconstant (Cstring s) }
| NIL
    { PEnil }
| LEFTPAR e = expr RIGHTPAR
  { e.pexpr_desc }
| id = ident
   { PEident id }
| e = expr DOT id = ident
   { PEdot (e, id) }
| id = ident; el = arguments
   { PEcall (id, el) }
| e = expr DOT id = ident; el = arguments
   { match e.pexpr_desc, id.id with
     | PEident {id="fmt"}, "Print" -> PEcall ({id with id = "fmt.Print"}, el)
     | _ -> raise Error }
| e1 = expr; op = binop; e2 = expr
  { PEbinop (op, e1, e2) }
| BANG; e1 = expr
  { PEunop (Unot, e1) }
| MINUS e1 = expr %prec UMINUS
  { PEunop (Uneg, e1) }
| AMP e1 = expr
  { PEunop (Uamp, e1) }
| STAR e1 = expr %prec USTAR
  { PEunop (Ustar, e1) }
;

arguments:
| LEFTPAR l = separated_list(COMMA, expr) RIGHTPAR
  { l }
;

%inline binop:
  | PLUS     { Badd }
  | MINUS    { Bsub }
  | STAR     { Bmul }
  | SLASH    { Bdiv }
  | PERCENT  { Bmod }
  | c = COMP { c }
  | AMPERSANDAMPERSAND     { Band }
  | VERTICALBARVERTICALBAR { Bor  }
;

ident:
  id = IDENT { { loc = $startpos, $endpos; id = id } }
;


