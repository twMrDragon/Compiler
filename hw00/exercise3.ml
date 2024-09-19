let rec palindrome m = 
  let l = String.length m in
  match l with
  | 0 | 1 -> true
  | _ -> m.[0]=m.[l-1] && palindrome (String.sub m 1 (l-2));;

let rec compare m1 m2 =
  let l1 = String.length m1 in
  let l2 = String.length m2 in
  match l1,l2 with
  | 0,0 -> false
  | 0,_ -> true
  | _,0 -> false
  | _,_ -> m1.[0]<m2.[0] || compare (String.sub m1 1 (l1-1)) (String.sub m2 1 (l2-1));;

let rec factor m1 m2 =
  let l1 = String.length m1 in
  let l2 = String.length m2 in
  if l2<l1 then 
    false
  else if m1=(String.sub m2 0 l1) then
    true
  else
    factor m1 (String.sub m2 1 (l2-1));;