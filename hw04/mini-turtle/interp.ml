
(* interpreter of mini-Turtle *)

open Ast

exception Error of string

let unbound_var s = raise (Error ("unbound variable " ^ s))
let unbound_procedure f = raise (Error ("unbound procedure " ^ f))
let bad_arity x = raise (Error ("bad arity for " ^ x))

(* table of global variables *)
let globals = Hashtbl.create 17

(* data structure for local variables *)
module Smap = Map.Make(String)

(* arithmetic expressions *)

let binop = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * ) (* :-) *)
  | Div -> (/)

let rec expr env = function
  | Econst n -> n
  | Evar x when Smap.mem x env -> Smap.find x env
  | Evar x when Hashtbl.mem globals x -> Hashtbl.find globals x
  | Evar x -> unbound_var x
  | Ebinop (op, e1, e2) -> binop op (expr env e1) (expr env e2)

(* procedure table *)
let procs = Hashtbl.create 17

(* instructions *)

let rec stmt env = function
  | Spenup ->
      Turtle.pen_up ()
  | Spendown ->
      Turtle.pen_down ()
  | Sforward e ->
      Turtle.forward (expr env e)
  | Sturn e ->
      Turtle.turn_left (expr env e)
  | Scolor c ->
      Turtle.set_color c
  | Sif (e, s1, s2) ->
      stmt env (if expr env e <> 0 then s1 else s2)
  | Srepeat (e, s) ->
      for i = 1 to expr env e do stmt env s done
  | Sblock sl ->
      List.iter (stmt env) sl
  | Scall (x, el) ->
      let p =
	try Hashtbl.find procs x with Not_found -> unbound_procedure x
      in
      let env' =
	try
	  List.fold_left2 (fun env' x e -> Smap.add x (expr env e) env')
	    Smap.empty p.formals el
	with Invalid_argument _ -> bad_arity x
      in
      stmt env' p.body

let prog p =
  List.iter (fun p -> Hashtbl.add procs p.name p) p.defs;
  stmt Smap.empty p.main;
  ignore (Graphics.read_key ());
  Graphics.close_graph ()





