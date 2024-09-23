let rec square_sum l =
  match l with
  | [] -> 0
  | hd::tl -> hd*hd+square_sum tl;;

square_sum [];;  (* Expected output: 0 *)
square_sum [5];;  (* Expected output: 25 *)
square_sum [1; 2; 3];;  (* Expected output: 14 *)
square_sum [-1; -2; -3];;  (* Expected output: 14 *)
square_sum [-1; 0; 2; 3];;  (* Expected output: 14 *)
square_sum [2; 2; 2];;  (* Expected output: 12 *)
square_sum [10; 20; 30];;  (* Expected output: 1400 *)

let square_sum_list l =
  List.fold_left (fun acc x-> acc+x*x) 0 l;;

square_sum_list [];;  (* Expected output: 0 *)
square_sum_list [5];;  (* Expected output: 25 *)
square_sum_list [1; 2; 3];;  (* Expected output: 14 *)
square_sum_list [-1; -2; -3];;  (* Expected output: 14 *)
square_sum_list [-1; 0; 2; 3];;  (* Expected output: 14 *)
square_sum_list [2; 2; 2];;  (* Expected output: 12 *)
square_sum_list [10; 20; 30];;  (* Expected output: 1400 *)

let find_opt x l =
  let rec aux i l= 
    match l with
    | [] -> None  
    | hd::tl -> 
      if hd=x then Some i
      else aux(i+1) tl 
  in
  aux 0 l;;

find_opt 3 [1; 2; 3; 4; 5];;  (* Expected output: Some 2 *)
find_opt 1 [1; 2; 3; 4; 5];;  (* Expected output: Some 0 *)
find_opt 6 [1; 2; 3; 4; 5];;  (* Expected output: None *)
find_opt 1 [];;  (* Expected output: None *)
find_opt 2 [1; 2; 3; 2; 4];;  (* Expected output: Some 1 *)
find_opt (-1) [1; 2; -1; 4];;  (* Expected output: Some 2 *)
find_opt "hello" ["hi"; "hello"; "world"];;  (* Expected output: Some 1 *)

let find_opt_list x l =
  List.find_index (fun (y) -> y=x) l;;

find_opt_list 3 [1; 2; 3; 4; 5];;  (* Expected output: Some 2 *)
find_opt_list 1 [1; 2; 3; 4; 5];;  (* Expected output: Some 0 *)
find_opt_list 6 [1; 2; 3; 4; 5];;  (* Expected output: None *)
find_opt_list 1 [];;  (* Expected output: None *)
find_opt_list 2 [1; 2; 3; 2; 4];;  (* Expected output: Some 1 *)
find_opt_list (-1) [1; 2; -1; 4];;  (* Expected output: Some 2 *)
find_opt_list "hello" ["hi"; "hello"; "world"];;  (* Expected output: Some 1 *)