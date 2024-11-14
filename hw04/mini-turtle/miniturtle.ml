
(* main program of the interpreter mini-Turtle *)

open Format
open Lexing

(* Compilation option, to stop at the end of the parser *)
let parse_only = ref false

(* Source and target file names *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

(* Compiler options that are displayed with --help *)
let options =
  ["--parse-only", Arg.Set parse_only,
   "  To do only the parsing phase"]

let usage = "usage: mini-turtle [option] file.logo"

(* locates an error by indicating the line and column *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  (* Command line parsing *)
  Arg.parse options (set_file ifile) usage;

  (* We check that the name of the source file has been correctly indicated *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

  (* This file must have the extension .logo *)
  if not (Filename.check_suffix !ifile ".logo") then begin
    eprintf "This file must have the extension .logo\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Opening the source file for reading *)
  let f = open_in !ifile in

  (* Creating a lexical analysis buffer *)
  let buf = Lexing.from_channel f in

  try
    (* Parsing: The Parser.prog function transforms the lexical buffer
       into an abstract syntax tree if no errors (lexical or
       syntactic) are detected.  The Lexer.token function is used by
       Parser.prog to obtain the next token. *)
    let p = Parser.prog Lexer.token buf in
    close_in f;

    (* We stop here if we only want to parse *)
    if !parse_only then exit 0;

    Interp.prog p
  with
    | Lexer.Lexing_error c ->
	(* Lexical error. We retrieve its absolute position and
           convert it into a line number *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Lexical error: %s@." c;
	exit 1
    | Parser.Error ->
	(* Syntax error. We retrieve its absolute position and convert
           it to a line number *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Syntactic error@.";
	exit 1
    | Interp.Error s->
	(* Error during interpretation *)
	eprintf "Error : %s@." s;
	exit 1
