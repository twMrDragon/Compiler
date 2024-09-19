type 'a seq =
| Elt of 'a
| Seq of 'a seq * 'a seq

let (@@) x y = Seq(x, y) ;;

let rec hd = function
  | Elt x -> x
  | Seq(x,_) -> hd x ;;

let rec tl = function
  | Elt _ -> raise (Failure "tl")
  | Seq(x,y) ->  
    match x with
    | Elt _ ->  y
    | Seq(_,_) -> tl x @@ y ;;

let rec mem a set = 
  match set with
  | Elt y -> a=y
  | Seq(y,z) ->  mem a y || mem a z

let rec rev = function
  | Elt x -> x
  | Seq(x,y) -> rev y @@ rev x

let rec map f = function
  | Elt x -> f x
  | Seq(x,y) -> map f x @@ map f y

let rec fold_left f init = function
  | Elt x-> f init x
  | Seq(x,y) -> fold_left f (fold_left f init x) y

let rec fold_right f l init = 
  match l with
  | Elt x -> f init x
  | Seq(x,y) -> fold_right f x (fold_right f l y)

let rec seq2list = function
  | Elt x -> [x]
  | Seq(x,y) -> List.concat [seq2list x;seq2list y] ;;

let rec seq2list_tail l =
  let rec aux acc = function
    | Elt x -> x::acc
    | Seq(x,y) -> aux (aux acc y) x
  in
  aux [] l ;;

let rec length = function
  | Elt e-> 1
  | Seq(x,y) -> length x + length y ;;

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

  let rec nth s n = 
    if n < 0 then raise (Invalid_argument "nth")
    else
      match s with
      | Elt x -> if n=0 then x else raise (Failure "nth")
      | Seq(x,y) ->
        let len = length x in
        if n<len then nth x n
        else nth y (n - len)