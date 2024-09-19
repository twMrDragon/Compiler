let rec fact n =
  match n with
  | 0 -> 1
  | _ -> n * fact (n-1);;

let rec nb_bit_pos n = 
  match n with
  | 0 -> 0
  | _ -> nb_bit_pos (n lsr 1) + n land 1;;