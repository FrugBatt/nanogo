
(* Analyseur lexical pour Go *)

{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

  let kwd_tbl = [
      "else", ELSE;
      "false", CST (Cbool false);
      "for", FOR;
      "func", FUNC;
      "if", IF;
      "import", IMPORT;
      "nil", NIL;
      "package", PACKAGE;
      "return", RETURN;
      "struct", STRUCT;
      "true", CST (Cbool true);
      "type", TYPE;
      "var", VAR;
  ]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
  let newlines s lexbuf =
    String.iter (fun c -> if c = '\n' then newline lexbuf) s

  let string_buffer = Buffer.create 1024

  let nosemicolon = ref true
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let hexa = digit | ['a'-'f' 'A'-'F']
let integer =
  digit+
| "0" ("x" | "X") hexa+
let space = ' ' | '\t'

rule next_token = parse
  | (space* '\n')+ as s
      { newlines s lexbuf;
        if !nosemicolon then next_token lexbuf else SEMICOLON }
  | space+
      { next_token lexbuf }
  | "//" [^'\n']* '\n'
      { newline lexbuf;
        if !nosemicolon then next_token lexbuf else SEMICOLON }
  | "/*"
      { comment lexbuf; next_token lexbuf }
  | ident as id
      { id_or_kwd id }
  | ';'
      { SEMICOLON }
  | ','
      { COMMA }
  | '.'
      { DOT }
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | '*'
      { STAR }
  | '/'
      { SLASH }
  | '%'
      { PERCENT }
  | "&"
      { AMP }
  | "&&"
      { AMPERSANDAMPERSAND }
  | "||"
      { VERTICALBARVERTICALBAR }
  | "++"
      { PLUSPLUS }
  | "--"
      { MINUSMINUS }
  | "!"
      { BANG }
  | "="
      { EQ }
  | ":="
      { COLONEQ }
  | ">"
      { COMP Bgt }
  | ">="
      { COMP Bge }
  | "<"
      { COMP Blt }
  | "<="
      { COMP Ble }
  | "=="
      { COMP Beq }
  | "!="
      { COMP Bne }
  | '('
      { LEFTPAR }
  | ')'
      { RIGHTPAR }
  | '{'
      { LEFTBRACE }
  | '}'
      { RIGHTBRACE }
  | integer as s
      { try let n = Int64.neg (Int64.of_string ("-" ^ s)) in CST (Cint n)
	with _ -> raise (Lexing_error "literal constant too large") }
  | '"'
      { STRING (string lexbuf) }
  | eof
      { EOF }
  | _ as c
      { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	string lexbuf }
  | "\\t"
      { Buffer.add_char string_buffer '\t';
	string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	string lexbuf }
  | "\\\\"
      { Buffer.add_char string_buffer '\\';
	string lexbuf }
  | "\\" (_ as c)
      { raise (Lexing_error ("illegal escape character " ^ String.make 1 c)) }
  | '\n'
      { raise (Lexing_error "unclosed string") }
  | _ as c
      { Buffer.add_char string_buffer c;
	string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

and comment = parse
  | "*/" { () }
  | "\n" { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexing_error "unterminated comment") }

{

  (* point-virgule automatique après ident, int, string, return, ), } *)
  let next_token lexbuf =
    let t = next_token lexbuf in
    match t with
    | IDENT _ | CST _ | STRING _ | NIL | RETURN
    | PLUSPLUS | MINUSMINUS | RIGHTPAR | RIGHTBRACE ->
       nosemicolon := false; t
    | _ -> nosemicolon := true; t

}
