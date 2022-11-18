
(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Usage

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.next_token lb in
    close_in c;

    if debug then begin
      let ast_dot_file = open_out (Filename.chop_suffix file ".go" ^ "_ast.dot") in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_ast f (not !no_pretty));
      close_out ast_dot_file
    end;

    if !parse_only then exit 0;

    let f = Typing.file ~debug f in

    if debug then begin
      let ast_dot_file = open_out (Filename.chop_suffix file ".go" ^ "_tast.dot") in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_tast f (not !no_pretty));
      close_out ast_dot_file
    end;

    if !type_only then exit 0;

    let code = Compile.file ~debug f in

    let c = open_out (Filename.chop_suffix file ".go" ^ ".s") in
    let fmt = formatter_of_out_channel c in
    X86_64.print_program fmt code;
    close_out c
  with
    | Lexer.Lexing_error s ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s\n@." s;
      exit 1
    | Parser.Error ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error\n@.";
      exit 1
    | Typing.Error (l, msg) ->
      report_loc l;
      eprintf "error: %s\n@." msg;
      exit 1
    | Typing.Anomaly msg ->
      eprintf "Typing Anomaly: %s\n@." msg;
      exit 2
    | Compile.Anomaly msg ->
      eprintf "Compile Anomaly: %s\n@." msg;
      exit 2
    | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2



