

open Ast
open Format

(* Exception raised to signal a runtime error *)
exception Error of string
let error s = raise (Error s)

(* Values of Mini-Python.

   Two main differences wrt Python:

   - We use here machine integers (OCaml type `int`) while Python
     integers are arbitrary-precision integers (we could use an OCaml
     library for big integers, such as zarith, but we opt for simplicity
     here).

   - What Python calls a ``list'' is a resizeable array. In Mini-Python,
     there is no way to modify the length, so a mere OCaml array can be used.
*)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Print a value on standard output *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Boolean interpretation of a value

   In Python, any value can be used as a Boolean: None, the integer 0,
   the empty string, and the empty list are all considered to be
   False, and any other value to be True.
*)
let is_false = function
  | Vnone
  | Vbool false
  | Vstring ""
  | Vlist [||] -> true
  | Vint n -> n = 0
  | _ -> false

let is_true v = not (is_false v)

(* Binary operators
   - Operator + is overloaded: arithmetic addition, string concatenation,
     and list concatenation.
   - Operators // and % must fail if we attempt a division by zero.
*)

let rec compare_list a1 n1 a2 n2 i =
  if i = n1 && i = n2 then 0
  else if i = n1 then -1
  else if i = n2 then 1
  else let c = compare a1.(i) a2.(i) in
       if c <> 0 then c else compare_list a1 n1 a2 n2 (i + 1)

let rec compare_value v1 v2 = match v1, v2 with
  | Vlist a1, Vlist a2 ->
    compare_list a1 (Array.length a1) a2 (Array.length a2) 0
  | _ -> compare v1 v2

let binop op v1 v2 = match op, v1, v2 with
  | Badd, Vint n1, Vint n2 -> Vint (n1+n2)
  | Badd, Vstring s1, Vstring s2 -> Vstring (s1 ^ s2)
  | Badd, Vlist l1, Vlist l2 -> Vlist (Array.append l1 l2)
  | Bsub, Vint n1, Vint n2 -> Vint (n1-n2)
  | Bmul, Vint n1, Vint n2 -> Vint (n1*n2)
  | (Bdiv | Bmod), Vint _, Vint 0 -> error "division by zero"
  | Bdiv, Vint n1, Vint n2 -> Vint (n1/n2)
  | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
  | Beq, _, _ -> Vbool (compare_value v1 v2 = 0)
  | Bneq, _, _ -> Vbool (compare_value v1 v2 <> 0)
  | Blt, _, _ -> Vbool (compare_value v1 v2 < 0)
  | Ble, _, _ -> Vbool (compare_value v1 v2 <= 0)
  | Bgt, _, _ -> Vbool (compare_value v1 v2 > 0)
  | Bge, _, _ -> Vbool (compare_value v1 v2 >= 0)
  | _ -> error "unsupported operand types"

(* We only have global functions in Mini-Python *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* The following exception is used to interpret Python's `return` *)

exception Return of value

(* Local variables (function parameters and local variables introduced
   by assignments) are stored in a hash table that is passed to the
   following OCaml functions as parameter `ctx`. *)

type ctx = (string, value) Hashtbl.t

(* Interpreting an expression (returns a value) *)

let rec expr ctx = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cbool b) ->
      Vbool b
  | Ecst (Cint n) ->
      Vint (Int64.to_int n)
  | Ecst (Cstring s) ->
      Vstring s
  | Ebinop (Band, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_true v1 then expr ctx e2 else v1
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_false v1 then expr ctx e2 else v1
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      binop op (expr ctx e1) (expr ctx e2)
  | Eunop (Uneg, e1) ->
      begin match expr ctx e1 with
        | Vint n -> Vint (-n)
        | _ -> error "unsupported operand types" end
  | Eunop (Unot, e1) ->
      Vbool (is_false (expr ctx e1))
  | Ecall ({id="len"}, [e1]) ->
      begin match expr ctx e1 with
        | Vstring s -> Vint (String.length s)
        | Vlist l -> Vint (Array.length l)
        | _ -> error "this value has no 'len'" end
  | Ecall ({id="list"}, [Ecall ({id="range"}, [e1])]) ->
      let n = expr_int ctx e1 in
      Vlist (Array.init (max 0 n) (fun i -> Vint i))
  | Ecall ({id=f}, el) ->
      if not (Hashtbl.mem functions f) then error ("unbound function " ^ f);
      let args, body = Hashtbl.find functions f in
      if List.length args <> List.length el then error "bad arity";
      let ctx' = Hashtbl.create 16 in
      List.iter2 (fun {id=x} e -> Hashtbl.add ctx' x (expr ctx e)) args el;
      begin try stmt ctx' body; Vnone with Return v -> v end
  | Elist el ->
      Vlist (Array.of_list (List.map (expr ctx) el))
  | Eident {id} ->
      if not (Hashtbl.mem ctx id) then error "unbound variable";
      Hashtbl.find ctx id
  | Eget (e1, e2) ->
      begin match expr ctx e1 with
      | Vlist l ->
          let i = expr_int ctx e2 in
          (try l.(i) with Invalid_argument _ -> error "index out of bounds")
      | _ -> error "list expected" end

(* Interpreting an expression, and checking that it is an integer *)

and expr_int ctx e = match expr ctx e with
  | Vbool false -> 0
  | Vbool true -> 1
  | Vint n -> n
  | _ -> error "integer expected"

(* Interpreting a statement

   returns nothing but may raise exception `Return` *)

and stmt ctx = function
  | Sif (e, s1, s2) ->
      if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
  | Sreturn e ->
      raise (Return (expr ctx e))
  | Sassign ({id}, e1) ->
      Hashtbl.replace ctx id (expr ctx e1)
  | Sset (e1, e2, e3) ->
      begin match expr ctx e1 with
      | Vlist l -> l.(expr_int ctx e2) <- expr ctx e3
      | _ -> error "list expected" end
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sfor ({id=x}, e, s) ->
      begin match expr ctx e with
      | Vlist l ->
        Array.iter (fun v -> Hashtbl.replace ctx x v; stmt ctx s) l
      | _ -> error "list expected" end
  | Seval e ->
      ignore (expr ctx e)

(* Interpreting a block (a sequence of statements) *)

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* Interpreting a file *)

let file (fl, s) =
  List.iter
    (fun (f,args,body) -> Hashtbl.add functions f.id (args, body)) fl;
  stmt (Hashtbl.create 16) s

