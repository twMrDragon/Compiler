(* Exercise 1 *)

(* 字元和他的 index *)
type ichar = char * int

type regexp =
  (* 空字串 *)
  | Epsilon
  (* 單個字符 *)
  | Character of ichar
  (* OR *)
  | Union of regexp * regexp
  (* AND *)
  | Concat of regexp * regexp
  (* * *)
  | Star of regexp

(* 判斷使否匹配空字串*)
let rec null = function
  | Epsilon -> true
  | Character _ -> false
  | Union (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2
  (* 0 或 1 次以上 *)
  | Star _ -> true

let () =
  let a = Character ('a', 0) in
  assert (not (null a));
  assert (null (Star a));
  assert (null (Concat (Epsilon, Star Epsilon)));
  assert (null (Union (Epsilon, a)));
  assert (not (null (Concat (a, Star a))))

(* Exercise 2 *)

(* 一組 set 包含多個 ichar *)
module Cset = Set.Make (struct
  type t = ichar

  let compare = Stdlib.compare
end)

(* 第一個字元可能出現的集合 *)
let rec first = function
  | Epsilon -> Cset.empty
  | Character c -> Cset.singleton c
  | Union (r1, r2) -> Cset.union (first r1) (first r2)
  | Concat (r1, r2) ->
      if null r1 then Cset.union (first r1) (first r2) else first r1
  | Star r -> first r

(* 最後一個字元可能出現的集合 *)
let rec last = function
  | Epsilon -> Cset.empty
  | Character c -> Cset.singleton c
  | Union (r1, r2) -> Cset.union (last r1) (last r2)
  | Concat (r1, r2) ->
      if null r2 then Cset.union (last r1) (last r2) else last r2
  | Star r -> last r

let () =
  let ca = ('a', 0) and cb = ('b', 0) in
  let a = Character ca and b = Character cb in
  let ab = Concat (a, b) in
  let eq = Cset.equal in
  assert (eq (first a) (Cset.singleton ca));
  assert (eq (first ab) (Cset.singleton ca));
  assert (eq (first (Star ab)) (Cset.singleton ca));
  assert (eq (last b) (Cset.singleton cb));
  assert (eq (last ab) (Cset.singleton cb));
  assert (Cset.cardinal (first (Union (a, b))) = 2);
  assert (Cset.cardinal (first (Concat (Star a, b))) = 2);
  assert (Cset.cardinal (last (Concat (a, Star b))) = 2)

(* Exercise 3 *)

(* 下一個字元可能出現的集合 *)
let rec follow c = function
  | Epsilon | Character _ -> Cset.empty
  | Union (r1, r2) -> Cset.union (follow c r1) (follow c r2)
  | Concat (r1, r2) ->
      let s = Cset.union (follow c r1) (follow c r2) in
      if Cset.mem c (last r1) then Cset.union s (first r2) else s
  | Star r ->
      if Cset.mem c (last r) then Cset.union (follow c r) (first r)
      else follow c r

let () =
  let ca = ('a', 0) and cb = ('b', 0) in
  let a = Character ca and b = Character cb in
  let ab = Concat (a, b) in
  assert (Cset.equal (follow ca ab) (Cset.singleton cb));
  assert (Cset.is_empty (follow cb ab));
  let r = Star (Union (a, b)) in
  assert (Cset.cardinal (follow ca r) = 2);
  assert (Cset.cardinal (follow cb r) = 2);
  let r2 = Star (Concat (a, Star b)) in
  assert (Cset.cardinal (follow cb r2) = 2);
  let r3 = Concat (Star a, b) in
  assert (Cset.cardinal (follow ca r3) = 2)

(* Exercise 4 *)

type state = Cset.t (* a state is a set of characters *)

module Cmap = Map.Make (Char) (* dictionary whose keys are characters *)
module Smap = Map.Make (Cset) (* dictionary whose keys are states *)

(* 產生能夠轉移得下個狀態集 *)
let next_state r q c =
  Cset.fold
    (fun ((c', _) as ci) q' ->
      if c' = c then Cset.union q' (follow ci r) else q')
    q Cset.empty

type autom = {
  start : state;
  trans : state Cmap.t Smap.t;
      (* state dictionary -> (character dictionary -> state) *)
}

let eof = ('#', -1)

(* 建立狀態 to 狀態 (向量機) *)
let make_dfa r =
  (* 補上結束字符 *)
  let r = Concat (r, Character eof) in
  (* transitions under construction *)
  (* 初始化狀態轉移字典 *)
  let trans = ref Smap.empty in

  let rec transitions q =
    (* the transitions function constructs all the transitions of the state q,
       if this is the first time q is visited *)
    (* 第一次遇到這個狀態 *)
    if not (Smap.mem q !trans) then (
      (* 初始化狀態轉移 *)
      trans := Smap.add q Cmap.empty !trans;
      (* 遍歷狀態 q 的字符 *)
      Cset.iter
        (fun (c, _) ->
          let t = Smap.find q !trans in
          if not (Cmap.mem c t) then (
            let q' = next_state r q c in
            trans := Smap.add q (Cmap.add c q' t) !trans;
            transitions q'))
        q)
  in

  let q0 = first r in
  transitions q0;
  { start = q0; trans = !trans }

let fprint_state fmt q =
  Cset.iter
    (fun (c, i) ->
      if c = '#' then Format.fprintf fmt "# "
      else Format.fprintf fmt "%c%i " c i)
    q

let fprint_transition fmt q c q' =
  Format.fprintf fmt "\"%a\" -> \"%a\" [label=\"%c\"];@\n" fprint_state q
    fprint_state q' c

let fprint_autom fmt a =
  Format.fprintf fmt "digraph A {@\n";
  Format.fprintf fmt " @[\"%a\" [ shape = \"rect\"];@\n" fprint_state a.start;
  Smap.iter
    (fun q t -> Cmap.iter (fun c q' -> fprint_transition fmt q c q') t)
    a.trans;
  Format.fprintf fmt "@]@\n}@."

let save_autom file a =
  let ch = open_out file in
  Format.fprintf (Format.formatter_of_out_channel ch) "%a" fprint_autom a;
  close_out ch
(* (a|b)*a(a|b) *)

let r =
  Concat
    ( Star (Union (Character ('a', 1), Character ('b', 1))),
      Concat (Character ('a', 2), Union (Character ('a', 3), Character ('b', 2)))
    )

let a = make_dfa r
let () = save_autom "autom.dot" a

(* Exercise 5 *)

let recognize a s =
  let n = String.length s in

  let rec loop q i =
    (* 是否結束 *)
    if i = n then Cset.mem eof q
    else
      let c = s.[i] in
      (* 找到下一個轉換 *)
      let t = Smap.find q a.trans in
      (* 檢查該字元在有在轉換中 *)
      loop (Cmap.find c t) (i + 1)
  in

  try loop a.start 0 with Not_found -> false

let () = assert (recognize a "aa")
let () = assert (recognize a "ab")
let () = assert (recognize a "abababaab")
let () = assert (recognize a "babababab")
let () = assert (recognize a (String.make 1000 'b' ^ "ab"))
let () = assert (not (recognize a "a"))
let () = assert (not (recognize a "b"))
let () = assert (not (recognize a "ba"))
let () = assert (not (recognize a "aba"))
let () = assert (not (recognize a "abababaaba"))

let r =
  Star
    (Union
       ( Star (Character ('a', 1)),
         Concat
           ( Character ('b', 1),
             Concat (Star (Character ('a', 2)), Character ('b', 2)) ) ))

let a = make_dfa r
let () = save_autom "autom2.dot" a
let () = assert (recognize a "")
let () = assert (recognize a "bb")
let () = assert (recognize a "aaa")
let () = assert (recognize a "aaabbaaababaaa")
let () = assert (recognize a "bbbbbbbbbbbbbb")
let () = assert (recognize a "bbbbabbbbabbbabbb")
let () = assert (not (recognize a "b"))
let () = assert (not (recognize a "ba"))
let () = assert (not (recognize a "ab"))
let () = assert (not (recognize a "aaabbaaaaabaaa"))
let () = assert (not (recognize a "bbbbbbbbbbbbb"))
let () = assert (not (recognize a "bbbbabbbbabbbabbbb"))

(* Exercise 6 *)

open Printf
open Format

let fprint_autom_ml fmt a =
  (* on numérote tous les états *)
  (* 每個狀態一個編號 *)
  let num =
    let n = ref 0 in
    Smap.fold
      (fun s _ num ->
        incr n;
        Smap.add s !n num)
      a.trans Smap.empty
  in
  (* 輸出轉換 *)
  let print_trans c s =
    if c <> '#' then fprintf fmt "| '%c' -> state%d b@\n" c (Smap.find s num)
  in
  let first = ref true in
  (* 輸出狀態 *)
  let print_state s tr =
    fprintf fmt "@[<hov 2>";
    (* 第一個狀態 *)
    if !first then (
      first := false;
      fprintf fmt "let rec ")
    else fprintf fmt "and ";
    (* 輸出當前狀態名 *)
    fprintf fmt "state%d b =@\n" (Smap.find s num);
    (* 檢查ㄉ前狀態是否為結束狀態 *)
    if Cset.mem eof s then fprintf fmt "b.last <- b.current;@\n";
    (* 輸出 match *)
    fprintf fmt "match next_char b with@\n";
    (* 輸出所有轉換 *)
    Cmap.iter print_trans tr;
    (* 輸出 error *)
    fprintf fmt "| _ -> failwith \"lexical error\"";
    fprintf fmt "@]@\n"
  in
  (* 遍歷並輸初每個狀態 *)
  Smap.iter print_state a.trans;
  (* 輸出開始狀態 *)
  fprintf fmt "@\nlet start = state%d@\n" (Smap.find a.start num)

(* 根據向量機建立詞法分析 *)
let generate file a =
  let ch = open_out file in
  (* 讓輸出能套用格式 *)
  let fmt = formatter_of_out_channel ch in
  fprintf fmt
    "@[\n\
     type buffer = { text: string; mutable current: int; mutable last: int }\n\
     let next_char b =\n\
    \  if b.current = String.length b.text then raise End_of_file;\n\
    \  let c = b.text.[b.current] in\n\
    \  b.current <- b.current + 1;\n\
    \  c\n";
  (* 建立 autom .ml file *)
  fprintf fmt "%a@]@." fprint_autom_ml a;
  close_out ch

(* let r3 = Concat (Star (Character ('a', 1)), Character ('b', 1)) *)

let r3 =
  Concat
    ( Union (Character ('b', 1), Epsilon),
      Concat
        ( Star (Concat (Character ('a', 1), Character ('b', 2))),
          Union (Character ('a', 2), Epsilon) ) )

let a = make_dfa r3
let () = generate "a.ml" a
