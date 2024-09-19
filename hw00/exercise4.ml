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
    merge (sort l1) (sort l2)
let list_to_sort = [5; 1; 9; 3; 7; 2] ;;

sort list_to_sort