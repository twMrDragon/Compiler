
open Format
open X86_64
open Ast

let debug = ref false

(* let str_counter = ref 0
let str_table = Hashtbl.create 1000 *)

let str_table = Hashtbl.create 16
let label_counter = ref 0

let var_table = Hashtbl.create 16
let var_counter = ref 0

let get_var_counter()=
  let c = !var_counter in
  var_counter := !var_counter + 1;
  c

let get_label_counter() = 
  let c = !label_counter in
  label_counter := !label_counter + 1;
  c

let generate_str s =
  if not (Hashtbl.mem str_table s) then
    let current_count = get_label_counter() in
    let label = Printf.sprintf "str_%d" current_count in
    Hashtbl.add str_table s label;
  else
    ();
  movq (reg rax) (reg rbx) ++  
  leaq (ind ~ofs:(16) rax) (rdi) ++
  movq (lab ("$"^(Hashtbl.find str_table s))) (reg rsi) ++
  call "my_strcpy"++
  movq (reg rbx) (reg rax)

let rec generate_expr = function
  | TEcst c ->
    (match c with
    |  Cnone ->
      movq (imm 16) (reg rdi)++
      call "my_malloc"++
      movq (imm 0) (ind ~ofs:(0) rax)++
      movq (imm 0) (ind ~ofs:(8) rax)
    | Cbool b ->
      movq (imm 16) (reg rdi)++
      call "my_malloc"++
      movq (imm 1) (ind ~ofs:(0) rax)++
      movq (imm (if b then 1 else 0)) (ind ~ofs:(8) rax)
    | Cint i -> 
      movq (imm 16) (reg rdi)++
      call "my_malloc"++
      movq (imm 2) (ind ~ofs:(0) rax)++
      movq (imm64 i) (ind ~ofs:(8) rax)
    | Cstring s -> 
      movq (imm (16+((String.length s)+1))) (reg rdi) ++
      call "my_malloc"++
      movq (imm 3) (ind ~ofs:(0) rax) ++
      movq (imm (String.length s)) (ind ~ofs:(8) rax) ++
      generate_str s
    )
  | TEvar var ->
    let offset = Hashtbl.find var_table var.v_name in
    movq (lab "$my_array") (reg rbx) ++
    movq (ind ~ofs:(offset) rbx) (reg rax)
  | TEbinop (op, expr1, expr2) -> (
      let e1 = generate_expr expr1 ++ movq (reg rax)(reg r15) in
      let e2 = generate_expr expr2 ++ movq (reg rax) (reg r14) in
      e1++e2++
      match op with
      | Badd ->
        (* int add *)
        
        movq (ind ~ofs:(8) r14) (reg rax) ++
        addq (reg rax) (ind ~ofs:( 8) r15) ++
        movq (reg r15) (reg rax)
        (* str add *)
      | Bsub -> 
        movq (ind ~ofs:(8) r14) (reg rax) ++
        subq (reg rax) (ind ~ofs:( 8) r15) ++
        movq (reg r15) (reg rax)
      | Bmul -> 
        movq (ind ~ofs:(8) r14) (reg rax) ++
        imulq (ind ~ofs:(8) r15) (reg rax) ++
        movq (reg rax) (ind ~ofs:(8) r15) ++
        movq (reg r15) (reg rax)
      | Bdiv ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cqto ++
        idivq (ind ~ofs:(8) r14) ++
        movq (reg rax) (ind ~ofs:(8) r15) ++
        movq (reg r15) (reg rax)
      | Bmod ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cqto ++
        idivq (ind ~ofs:(8) r14) ++
        movq (reg rdx) (ind ~ofs:(8) r15) ++
        movq (reg r15) (reg rax)
      | Beq ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cmpq (reg rax)(ind ~ofs:(8) r14) ++
        sete (reg al) ++
        movzbq (reg al) rax ++
        movq (reg rax) (reg rbx)++
        (generate_expr (TEcst (Cbool true)))++
        (* generate true to rax *)
        movq (reg rbx) (ind ~ofs:(8) rax)
      | Bneq ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cmpq (reg rax)(ind ~ofs:(8) r14) ++
        setne (reg al) ++
        movzbq (reg al) rax ++
        movq (reg rax) (reg rbx)++
        (generate_expr (TEcst (Cbool true)))++
        (* generate true to rax *)
        movq (reg rbx) (ind ~ofs:(8) rax)
      | Blt ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cmpq (reg rax)(ind ~ofs:(8) r14) ++
        setg (reg al) ++
        movzbq (reg al) rax ++
        movq (reg rax) (reg rbx)++
        (generate_expr (TEcst (Cbool true)))++
        (* generate true to rax *)
        movq (reg rbx) (ind ~ofs:(8) rax)
      | Ble ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cmpq (reg rax)(ind ~ofs:(8) r14) ++
        setge (reg al) ++
        movzbq (reg al) rax ++
        movq (reg rax) (reg rbx)++
        (generate_expr (TEcst (Cbool true)))++
        (* generate true to rax *)
        movq (reg rbx) (ind ~ofs:(8) rax)
      | Bgt ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cmpq (reg rax)(ind ~ofs:(8) r14) ++
        setl (reg al) ++
        movzbq (reg al) rax ++
        movq (reg rax) (reg rbx)++
        (generate_expr (TEcst (Cbool true)))++
        (* generate true to rax *)
        movq (reg rbx) (ind ~ofs:(8) rax)
      | Bge ->
        movq (ind ~ofs:(8) r15) (reg rax) ++
        cmpq (reg rax)(ind ~ofs:(8) r14) ++
        setle (reg al) ++
        movzbq (reg al) rax ++
        movq (reg rax) (reg rbx)++
        (generate_expr (TEcst (Cbool true)))++
        (* generate true to rax *)
        movq (reg rbx) (ind ~ofs:(8) rax)
      |Band->
        movq (ind ~ofs:(8) r14) (reg rax) ++
        andq (reg rax) (ind ~ofs:(8) r15) ++
        movq (reg r15) (reg rax)
      | Bor->
        movq (ind ~ofs:(8) r14) (reg rax) ++
        orq (reg rax) (ind ~ofs:(8) r15) ++
        movq (reg r15) (reg rax))
  | TEunop (op,expr)->
    let e = generate_expr expr in
    e++
    (match op with
    | Uneg ->
      negq (ind ~ofs:(8) rax)
    | Unot ->
      notq (ind ~ofs:(8) rax)++
      andq (imm 1) (ind ~ofs:(8) rax))
  | TEcall (fn,exprs)->
    (match fn.fn_name with
    | "len" ->(
      let first = List.hd exprs in 
      (generate_expr first) ++
      movq (ind ~ofs:(8) rax) (reg rbx)++
      (generate_expr (TEcst (Cint 0L))) ++
      movq (reg rbx) (ind ~ofs:(8) rax)
      )
    | _ ->call fn.fn_name)
  | _ -> nop

let my_print =
  let label_int = Printf.sprintf "print_int_end_%d" (get_label_counter()) in
  let label_bool = Printf.sprintf "print_bool_end_%d" (get_label_counter()) in
  let label_false = Printf.sprintf "print_false_end_%d" (get_label_counter()) in
  let label_None = Printf.sprintf "print_none_end_%d" (get_label_counter()) in
  let label_str = Printf.sprintf "print_str_end_%d" (get_label_counter()) in
  let label_print_end = Printf.sprintf "print_end_%d" (get_label_counter()) in
  label "my_print" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  movq (reg rdi) (reg rbx) ++
  movq (ind ~ofs:(0) rbx) (reg rax) ++
  cmpq (imm 2) (reg rax) ++
  jne label_int ++
  movq (ind ~ofs:(8) rbx) (reg rsi) ++
  movq (lab "$format_int") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp label_print_end ++
  label label_int ++

  movq (ind ~ofs:(8) rbx) (reg rsi) ++
  cmpq (imm 1) (reg rax) ++
  jne label_bool ++
  cmpq (imm 1) (reg rsi) ++
  jne label_false ++
  movq (lab "$True") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp label_print_end ++
  label label_false++
  movq (lab "$False") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp label_print_end ++
  label label_bool++

  cmpq (imm 0) (reg rax) ++
  jne label_None ++
  movq (lab "$None") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp label_print_end ++
  label label_None ++

  cmpq (imm 3) (reg rax) ++
  jne label_str ++
  movq (lab "$format_string") (reg rdi) ++
  leaq (ind ~ofs:(16) rbx) (rsi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp label_print_end ++
  label label_str++
  
  label label_print_end ++
  movq (reg rbp) (reg rsp)++
  popq rbp ++
  movq (imm 0) (reg rax) ++
  ret

let rec generate_stmt stmt =
  match stmt with
  | TSif (expr,stmt1,stmt2)->
    let e = generate_expr expr in
    let cond_label = Printf.sprintf "cond_%d" (get_label_counter()) in
    let then_label = Printf.sprintf "then_%d" (get_label_counter()) in
    let else_label = Printf.sprintf "else_%d" (get_label_counter()) in
    let end_label = Printf.sprintf "end_%d" (get_label_counter()) in
    e++
    cmpq (imm 0) (ind ~ofs:(8) rax)  ++
    je else_label ++
    label then_label ++
    (generate_stmt stmt1) ++
    jmp end_label ++
    label else_label ++ 
    (generate_stmt stmt2) ++
    jmp end_label ++
    label end_label 
  | TSreturn e ->
    generate_expr e++
    popq rbp++
    ret
  | TSassign (id,expr) ->
    let e = generate_expr expr in
    if Hashtbl.mem var_table id.v_name then
      let offset = Hashtbl.find var_table id.v_name in
      e ++
      movq (lab "$my_array") (reg rbx) ++
      movq (reg rax) (ind ~ofs:(offset) rbx)
    else
      let offset = get_var_counter()*8 in
      Hashtbl.add var_table id.v_name offset;
      e ++
      movq (lab "$my_array") (reg rbx) ++
      movq (reg rax) (ind ~ofs:(offset) rbx)
  | TSprint expr->
  generate_expr expr ++
  movq (reg rax) (reg rdi) ++
  call "my_print"
  | TSblock stmts ->
    List.fold_left (fun acc stmt -> acc ++ (generate_stmt stmt)) nop stmts
  | TSfor (id,expr,stmt) -> nop
  | TSeval e ->
    generate_expr e
  | TSset (e1,e2,e3) -> nop

let generate_def def =
  let f,stmt = def in
  let label = label f.fn_name in
  let prologue = pushq (reg rbp) ++ movq (reg rsp) (reg rbp) in
  let epilogue = (generate_expr (TEcst Cnone))++
  movq (reg rbp)  (reg rsp) ++ 
  popq rbp ++
  (if f.fn_name = "main" then
    movq (imm 0) (reg rax)
  else
    nop)++
  ret in
  let body = generate_stmt stmt in
  label ++ prologue ++ body ++ epilogue

let rec generate_text_section = function
  | [] -> nop
  | def::tl ->
    let f,stmt = def in
    (generate_def def) ++ (generate_text_section tl)

let generate_data_section table =
    label "format_int" ++
    string "%d\n" ++
    label "format_string" ++
    string "%s\n" ++    
    label "True" ++
    string "True\n" ++
    label "False" ++
    string "False\n" ++
    label "None" ++
    string  "None\n" ++
    label "my_array" ++
    space 80000 ++ 
    Hashtbl.fold (fun key value acc ->
      acc ++ label value ++ string key) str_table nop

let my_malloc =
  label "my_malloc" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  call "malloc" ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let my_strcpy =
  label "my_strcpy" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  call "strcpy" ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let util_function = 
  my_malloc ++
  my_print ++
  my_strcpy

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let text = generate_text_section p in
  let data = generate_data_section str_table in
  { text = globl "main" ++  text ++ util_function;   (* TODO *)
    data = data; }                                 (* TODO *)