type 'a seq =
| Elt of 'a
| Seq of 'a seq * 'a seq

let (@@) x y = Seq(x, y);;

let seq1 = Elt 1 @@ Elt 2 @@ Elt 3 @@ Elt 4 @@ Elt 5
let seq2 = (((Elt 1 @@ Elt 2) @@ Elt 3) @@ Elt 4) @@ Elt 5
let seq3 = (Elt 5 @@ Elt 4) @@ (Elt 3 @@ (Elt 2 @@ Elt 1))
let seq4 = Elt 'h' @@ Elt 'e' @@ Elt 'l' @@ Elt 'l' @@ Elt 'o'

let rec hd = function
  | Elt x -> x
  | Seq(x,_) -> hd x;;

hd seq1;;  (* Expected output: 1 *)
hd seq2;;  (* Expected output: 1 *)
hd seq3;;  (* Expected output: 5 *)
hd seq4;;  (* Expected output: 'h' *)

let rec tl = function
  | Elt _ -> raise (Failure "tl")
  | Seq(x,y) ->  
    match x with
    | Elt _ ->  y
    | Seq(_,_) -> (tl x) @@ y;;

tl seq1;;  (* Expected output: Seq (Elt 2, Seq (Elt 3, Seq (Elt 4, Elt 5))) *)
tl seq2;;  (* Expected output: Seq (Seq (Seq (Elt 2, Elt 3), Elt 4), Elt 5) *)
tl seq3;;  (* Expected output: Seq (Elt 4, Seq (Elt 3, Seq (Elt 2, Elt 1))) *)
tl seq4;;  (* Expected output: Seq (Elt 'e', Seq (Elt 'l', Seq (Elt 'l', Elt 'o'))) *)

let rec mem a set = 
  match set with
  | Elt y -> a=y
  | Seq(y,z) ->  (mem a y) || (mem a z);;

mem 1 seq1;;  (* Expected output: true *)
mem 6 seq1;;  (* Expected output: false *)
mem 1 seq2;;  (* Expected output: true *)
mem 6 seq2;;  (* Expected output: false *)
mem 1 seq3;;  (* Expected output: true *)
mem 6 seq3;;  (* Expected output: false *)
mem 'h' seq4;;  (* Expected output: true *)
mem 'a' seq4;; (* Expected output: false *)

let rec rev = function
  | Elt x -> Elt x
  | Seq(x,y) -> (rev y) @@ (rev x);;

rev seq1;;  (* Expected output: Seq (Seq (Seq (Seq (Elt 5, Elt 4), Elt 3), Elt 2), Elt 1) *)
rev seq2;;  (* Expected output: Seq (Elt 5, Seq (Elt 4, Seq (Elt 3, Seq (Elt 2, Elt 1)))) *)
rev seq3;;  (* Expected output: Seq (Seq (Seq (Elt 1, Elt 2), Elt 3), Seq (Elt 4, Elt 5)) *)
rev seq4;;  (* Expected output: Seq (Seq (Seq (Seq (Elt 'o', Elt 'l'), Elt 'l'), Elt 'e'), Elt 'h') *)

let rec map f = function
  | Elt x -> Elt (f x)
  | Seq(x,y) -> (map f x) @@ (map f y);;

map (fun (x) -> x+1) seq1;;  (* Expected output: Seq (Elt 2, Seq (Elt 3, Seq (Elt 4, Seq (Elt 5, Elt 6)))) *)
map (fun (x) -> 5*x) seq2;;  (* Expected output: Seq (Seq (Seq (Seq (Elt 5, Elt 10), Elt 15), Elt 20), Elt 25) *)
map (fun (x) -> x*x) seq3;;  (* Expected output: Seq (Seq (Elt 25, Elt 16), Seq (Elt 9, Seq (Elt 4, Elt 1))) *)
map (fun (x) -> char_of_int(int_of_char x+1)) seq4;;  (* Expected output: Seq (Elt 'i', Seq (Elt 'f', Seq (Elt 'm', Seq (Elt 'm', Elt 'p')))) *)

let rec fold_left f init = function
  | Elt x-> f init x
  | Seq(x,y) -> fold_left f (fold_left f init x) y;;

fold_left (+) 0 seq1;;  (* Expected output: 15 *)
fold_left (-) 0 seq2;;  (* Expected output: -15 *)
fold_left (fun acc x -> acc*x) 1 seq3;;  (* Expected output: 120 *)
fold_left (fun acc x -> acc^(String.make 1 x)) "" seq4;;  (* Expected output: "hello" *)

let rec fold_right f l init = 
  match l with
  | Elt x -> f init x
  | Seq(x,y) -> fold_right f x (fold_right f y init);;

fold_right (+) seq1 0;;  (* Expected output: 15 *)
fold_right (-) seq2 0;;  (* Expected output: -15 *)
fold_right (fun acc x -> acc*x) seq3 2;;  (* Expected output: 240 *)
fold_right (fun acc x -> acc^(String.make 1 x)) seq4 "";;  (* Expected output: "olleh" *)

let rec seq2list = function
  | Elt x -> [x]
  | Seq(x,y) -> List.concat [seq2list x;seq2list y];;

seq2list seq1;;  (* Expected output: [1; 2; 3; 4; 5] *)
seq2list seq2;;  (* Expected output: [1; 2; 3; 4; 5] *)
seq2list seq3;;  (* Expected output: [5; 4; 3; 2; 1] *)
seq2list seq4;;  (* Expected output: ['h'; 'e'; 'l'; 'l'; 'o'] *)

let rec seq2list_tail l =
  let rec aux acc = function
    | Elt x -> x::acc
    | Seq(x,y) -> aux (aux acc y) x
  in
  aux [] l;;

seq2list_tail seq1;;  (* Expected output: [1; 2; 3; 4; 5] *)
seq2list_tail seq2;;  (* Expected output: [1; 2; 3; 4; 5] *)
seq2list_tail seq3;;  (* Expected output: [5; 4; 3; 2; 1] *)
seq2list_tail seq4;;  (* Expected output: ['h'; 'e'; 'l'; 'l'; 'o'] *)

let rec length = function
  | Elt e-> 1
  | Seq(x,y) -> length x + length y;;

let find_opt x l =
  let rec aux acc l=
    match l with
    | Elt y -> if x=y then Some acc else None
    | Seq(y,z) ->
      match aux acc y with
      | None -> aux (acc + length y) z
      | Some i -> Some i
  in
  aux 0 l;;

find_opt 1 seq1;;  (* Expected output: Some 0 *)
find_opt 6 seq1;;  (* Expected output: None *)
find_opt 2 seq2;;  (* Expected output: Some 1 *)
find_opt 6 seq2;;  (* Expected output: None *)
find_opt 1 seq3;;  (* Expected output: Some 4 *)
find_opt 6 seq3;;  (* Expected output: None *)
find_opt 'h' seq4;;  (* Expected output: Some 0 *)
find_opt 'a' seq4;;  (* Expected output: None *)

let rec nth s n = 
  if n < 0 then raise (Invalid_argument "nth")
  else
    match s with
    | Elt x -> if n=0 then x else raise (Failure "nth")
    | Seq(x,y) ->
      let len = length x in
      if n<len then nth x n
      else nth y (n - len);;

nth seq1 0;;  (* Expected output: 1 *)
nth seq1 1;;  (* Expected output: 2 *)
nth seq2 2;;  (* Expected output: 3 *)
nth seq2 3;;  (* Expected output: 4 *)
nth seq3 4;;  (* Expected output: 5 *)
nth seq3 5;;  (* Expected output: Exception: Failure "nth" *)
nth seq4 1;;  (* Expected output: 'e' *)
nth seq4 3;;  (* Expected output: 'l' *)