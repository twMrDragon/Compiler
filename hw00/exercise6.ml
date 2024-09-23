let rec range i j =
  if i>j then []
  else i::range (i+1) j

let l = range 0 1_000_000

let rev l =
  let rec aux acc = function
  | [] -> acc
  | hd::tl -> aux (hd::acc) tl
  in
  aux [] l;;

rev l

let rec map f l =
  let rec aux acc = function
  | [] -> rev acc
  | hd::tl -> aux (f hd::acc) tl
  in
  aux [] l;;

map (fun (x) -> 5*x) l