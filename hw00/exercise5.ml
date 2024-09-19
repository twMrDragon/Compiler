let rec square_sum l =
  match l with
  | [] -> 0
  | hd::tl -> hd*hd+square_sum tl ;;

let square_sum l =
  l
  |> List.map (fun x -> x*x)
  |> List.fold_left (+) 0 ;;

let find_opt x l =
  let rec aux i l= 
    match l with
    | [] -> None  
    | hd::tl -> 
      if hd=x then Some i
      else aux(i+1) tl 
  in
  aux 0 l;;

let find_opt x l =
  List.find_index (fun (y) -> y=x) l ;;

find_opt 2 [1;2;3;4;2]