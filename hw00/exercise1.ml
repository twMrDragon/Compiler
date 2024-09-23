let rec fact n =
  match n with
  | 0 -> 1
  | _ -> n * fact (n-1);;

fact 0;;  (* Expected output: 1 *)
fact 1;;  (* Expected output: 1 *)
fact 5;;  (* Expected output: 120 *)
fact 10;;  (* Expected output: 3628800 *)

let rec nb_bit_pos n = 
  match n with
  | 0 -> 0
  | _ -> nb_bit_pos (n lsr 1) + n land 1;;

nb_bit_pos 0;;  (* Expected output: 0 *)
nb_bit_pos 1;;  (* Expected output: 1 *)
nb_bit_pos 2;;  (* Expected output: 1 *)
nb_bit_pos 4;;  (* Expected output: 1 *)
nb_bit_pos 7;;  (* Expected output: 3 *)
nb_bit_pos 15;;  (* Expected output: 4 *)
nb_bit_pos 19;;  (* Expected output: 3 *)
nb_bit_pos 255;;  (* Expected output: 8 *)