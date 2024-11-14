
(* Lexical analyser for mini-Turtle *)

{
  open Lexing
  open Parser

  (* raise exception to report a lexical error *)
  exception Lexing_error of string

  (* note : remember to call the Lexing.new_line function
at each carriage return ('\n' character) *)
  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      [
        "forward",FORWARD;
        "penup",PENUP;
        "pendown",PENDOWN;
        "turnleft",TURNLEFT;
        "turnright",TURNRIGHT;
        "color",COLOR;
        "black",BLACK;
        "white",WHITE;
        "red",RED;
        "green",GREEN;
        "blue",BLUE;
        "if",IF;
        "else",ELSE;
        "repeat",REPEAT;
        "def",DEF;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter(letter|digit|'_')*
let integer = ['0'-'9']+
let space = ' ' | '\t' | '\r'

rule token = parse
  | "//" [^'\n']* '\n'
  | "\n" {new_line lexbuf; token lexbuf}
  | "(*" {comment lexbuf}
  | space+ {token lexbuf}
  | ident as id {id_or_kwd id}
  | integer as s { CST (int_of_string s) }
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {TIMES}
  | '/' {DIV}
  | '(' {LP}
  | ')' {RP}
  | '{' {BEGIN}
  | '}' {END}
  | ',' {COMMA}
  | "//" [^'\n']* eof
  | eof {EOF}
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)" {token lexbuf}
  | '\n' {new_line lexbuf; comment lexbuf}
  | _ {comment lexbuf}
  | eof {raise (Lexing_error ("unsealed comment"))}