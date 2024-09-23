let fibo n =
  let rec aux n a b=
    if n=0 then a
    else aux (n-1) b (a+b)
  in
  aux n 0 1;;

fibo 0;;  (* Expected output: 0 *)
fibo 1;;  (* Expected output: 1 *)
fibo 5;;  (* Expected output: 5 *)
fibo 10;;  (* Expected output: 55 *)
fibo 20;;  (* Expected output: 6765 *)
fibo 30;;  (* Expected output: 832040 *)