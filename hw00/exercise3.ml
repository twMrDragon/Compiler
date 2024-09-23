let rec palindrome m = 
  let l = String.length m in
  match l with
  | 0 | 1 -> true
  | _ -> m.[0]=m.[l-1] && palindrome (String.sub m 1 (l-2));;

palindrome "a";;   (* Expected output: true *)
palindrome "";;   (* Expected output: true *)
palindrome "madam";;   (* Expected output: true *)
palindrome "racecar";; (* Expected output: true *)
palindrome "hello";;   (* Expected output: false *)
palindrome "world";;   (* Expected output: false *)
palindrome "abba";;    (* Expected output: true *)
palindrome "noon";;    (* Expected output: true *)
palindrome "121";;   (* Expected output: true *)
palindrome "12321";; (* Expected output: true *)
palindrome "123";;   (* Expected output: false *)
palindrome "456";;   (* Expected output: false *)

let rec compare m1 m2 =
  let l1 = String.length m1 in
  let l2 = String.length m2 in
  match l1,l2 with
  | 0,0 -> false
  | 0,_ -> true
  | _,0 -> false
  | _,_ -> 
    if m1.[0]<m2.[0] then true
    else if m1.[0]>m2.[0] then false
    else compare (String.sub m1 1 (l1-1)) (String.sub m2 1 (l2-1));;

compare "apple" "banana";;   (* Expected output: true *)
compare "banana" "apple";;   (* Expected output: false *)
compare "apple" "apple";;    (* Expected output: false *)
compare "" "apple";;         (* Expected output: true *)
compare "apple" "";;         (* Expected output: false *)

let rec factor m1 m2 =
  let l1 = String.length m1 in
  let l2 = String.length m2 in
  if l2<l1 then 
    false
  else if m1=(String.sub m2 0 l1) then
    true
  else
    factor m1 (String.sub m2 1 (l2-1));;

factor "app" "banana apple";;   (* Expected output: true *)
factor "apple" "apple";;         (* Expected output: true *)
factor "cat" "banana apple";;    (* Expected output: false *)
factor "" "banana apple";;       (* Expected output: true *)
factor "cat" "";                 (* Expected output: false *)