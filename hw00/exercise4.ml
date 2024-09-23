let rec split l =
  match l with
  | [] -> ([],[])
  | [x] -> ([x],[])
  | x::y::tl ->
    let (l1,l2) = split tl in
    (x::l1, y::l2)

let rec merge l1 l2=
  match l1,l2 with
  | [],l | l,[] -> l
  | x1::tl1,x2::tl2 -> 
    if x1<x2 then
      x1::merge tl1 l2
    else
      x2::merge l1 tl2
      
let rec sort l=
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let l1,l2 = split l in
    merge (sort l1) (sort l2);;

sort [];;  (* Expected output: [] *)
sort [42];;  (* Expected output: [42] *)
sort [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5];;  (* Expected output: [1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9] *)
sort [1; 2; 3; 4; 5];;  (* Expected output: [1; 2; 3; 4; 5] *)
sort [5; 5; 3; 1; 2; 2];;  (* Expected output: [1; 2; 2; 3; 5; 5] *)
sort [-1; -3; 0; -2];;  (* Expected output: [-3; -2; -1; 0] *)
sort [-5; 1; 0; 3; -2; -1];;  (* Expected output: [-5; -2; -1; 0; 1; 3] *)