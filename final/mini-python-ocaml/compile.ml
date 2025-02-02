open Format
open X86_64
open Ast

let debug = ref false

(* let str_counter = ref 0
let str_table = Hashtbl.create 1000 *)

let global_str_table = Hashtbl.create 16
let label_counter = ref 0
let global_var_table = Hashtbl.create 16
let var_counter = ref 0

let get_var_counter () =
  let c = !var_counter in
  var_counter := !var_counter + 1;
  c

let get_label_counter () =
  let c = !label_counter in
  label_counter := !label_counter + 1;
  c

let generate_str str_table s =
  if not (Hashtbl.mem str_table s) then
    let current_count = get_label_counter () in
    let label = Printf.sprintf "str_%d" current_count in
    Hashtbl.add str_table s label
  else ();
  pushq (reg rax)
  ++ pushq (imm 0)
  ++ leaq (ind ~ofs:16 rax) rdi
  ++ movq (lab ("$" ^ Hashtbl.find str_table s)) (reg rsi)
  ++ call "my_strcpy" ++ popq rax ++ popq rax

let rec generate_expr var_table str_table = function
  | TEcst c -> (
      match c with
      | Cnone ->
          movq (imm 16) (reg rdi)
          ++ call "my_malloc"
          ++ movq (imm 0) (ind ~ofs:0 rax)
          ++ movq (imm 0) (ind ~ofs:8 rax)
      | Cbool b ->
          movq (imm 16) (reg rdi)
          ++ call "my_malloc"
          ++ movq (imm 1) (ind ~ofs:0 rax)
          ++ movq (imm (if b then 1 else 0)) (ind ~ofs:8 rax)
      | Cint i ->
          movq (imm 16) (reg rdi)
          ++ call "my_malloc"
          ++ movq (imm 2) (ind ~ofs:0 rax)
          ++ movq (imm64 i) (ind ~ofs:8 rax)
      | Cstring s ->
          movq (imm (16 + (String.length s + 1))) (reg rdi)
          ++ call "my_malloc"
          ++ movq (imm 3) (ind ~ofs:0 rax)
          ++ movq (imm (String.length s)) (ind ~ofs:8 rax)
          ++ generate_str str_table s)
  | TEvar var ->
      let offset = Hashtbl.find var_table var.v_name in
      movq (lab "$my_array") (reg rbx) ++ movq (ind ~ofs:offset rbx) (reg rax)
  | TEbinop (op, expr1, expr2) -> (
      generate_expr var_table str_table expr1
      ++ pushq (reg rax)
      ++ generate_expr var_table str_table expr2
      ++ pushq (reg rax)
      ++
      match op with
      | Badd ->
          (* int add *)
          let label_str_add =
            Printf.sprintf "str_add_%d" (get_label_counter ())
          in
          let label_add_end =
            Printf.sprintf "add_end_%d" (get_label_counter ())
          in
          cmpq (imm 3) (ind ~ofs:0 rax)
          ++ je label_str_add
          ++ generate_expr var_table str_table (TEcst (Cint 0L))
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rbx)
          ++ addq (ind ~ofs:8 r14) (reg rbx)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
          ++ jmp label_add_end
          (* str add *)
          ++ label label_str_add
          ++ popq r14 ++ popq r15
          ++ pushq (reg r15)
          ++ pushq (reg r14)
          ++ movq (imm 16) (reg rdi)
          ++ addq (ind ~ofs:8 r15) (reg rdi)
          ++ addq (ind ~ofs:8 r14) (reg rdi)
          ++ call "my_malloc"
          ++ movq (imm 3) (ind ~ofs:0 rax)
          ++ movq (reg rdi) (ind ~ofs:8 rax)
          ++ popq r14 ++ popq r15
          ++ pushq (reg r15)
          ++ pushq (reg r14)
          ++ pushq (reg rax)
          ++ pushq (imm 0)
          ++ leaq (ind ~ofs:16 rax) rdi
          ++ leaq (ind ~ofs:16 r15) rsi
          ++ call "my_strcpy" ++ popq rax ++ popq rax ++ popq r14 ++ popq r15
          ++ pushq (reg r15)
          ++ pushq (reg r14)
          ++ pushq (reg rax)
          ++ pushq (imm 0)
          ++ leaq (ind ~ofs:16 rax) rdi
          ++ leaq (ind ~ofs:16 r14) rsi
          ++ call "my_strcat" ++ popq rax ++ popq rax ++ popq r14 ++ popq r15
          ++ label label_add_end
      | Bsub ->
          generate_expr var_table str_table (TEcst (Cint 0L))
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rbx)
          ++ subq (ind ~ofs:8 r14) (reg rbx)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Bmul ->
          generate_expr var_table str_table (TEcst (Cint 0L))
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rbx)
          ++ imulq (ind ~ofs:8 r14) (reg rbx)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Bdiv ->
          generate_expr var_table str_table (TEcst (Cint 0L))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cqto
          ++ idivq (ind ~ofs:8 r14)
          ++ movq (reg rax) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Bmod ->
          generate_expr var_table str_table (TEcst (Cint 0L))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cqto
          ++ idivq (ind ~ofs:8 r14)
          ++ movq (reg rdx) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Beq ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cmpq (reg rax) (ind ~ofs:8 r14)
          ++ sete (reg al)
          ++ movzbq (reg al) rax
          ++ movq (reg rax) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Bneq ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cmpq (reg rax) (ind ~ofs:8 r14)
          ++ setne (reg al)
          ++ movzbq (reg al) rax
          ++ movq (reg rax) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Blt ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cmpq (reg rax) (ind ~ofs:8 r14)
          ++ setg (reg al)
          ++ movzbq (reg al) rax
          ++ movq (reg rax) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Ble ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cmpq (reg rax) (ind ~ofs:8 r14)
          ++ setge (reg al)
          ++ movzbq (reg al) rax
          ++ movq (reg rax) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Bgt ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cmpq (reg rax) (ind ~ofs:8 r14)
          ++ setl (reg al)
          ++ movzbq (reg al) rax
          ++ movq (reg rax) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Bge ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ movq (reg rax) (reg rcx)
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r15) (reg rax)
          ++ cmpq (reg rax) (ind ~ofs:8 r14)
          ++ setle (reg al)
          ++ movzbq (reg al) rax
          ++ movq (reg rax) (reg rbx)
          ++ movq (reg rcx) (reg rax)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Band ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r14) (reg rbx)
          ++ andq (ind ~ofs:8 r15) (reg rbx)
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | Bor ->
          generate_expr var_table str_table (TEcst (Cbool true))
          ++ popq r14 ++ popq r15
          ++ movq (ind ~ofs:8 r14) (reg rbx)
          ++ orq (ind ~ofs:8 r15) (reg rbx)
          ++ movq (reg rbx) (ind ~ofs:8 rax))
  | TEunop (op, expr) -> (
      generate_expr var_table str_table expr
      ++
      match op with
      | Uneg -> negq (ind ~ofs:8 rax)
      | Unot -> notq (ind ~ofs:8 rax) ++ andq (imm 1) (ind ~ofs:8 rax))
  | TEcall (fn, exprs) -> (
      match fn.fn_name with
      | "len" ->
          let first = List.hd exprs in
          generate_expr var_table str_table first
          ++ movq (ind ~ofs:8 rax) (reg rbx)
          ++ generate_expr var_table str_table (TEcst (Cint 0L))
          ++ movq (reg rbx) (ind ~ofs:8 rax)
      | _ ->
          let paired_list = List.combine fn.fn_params exprs in
          let get_offset table name =
            if Hashtbl.mem table name then Hashtbl.find table name
            else
              let offset = get_var_counter () * 8 in
              Hashtbl.add table name offset;
              offset
          in
          List.fold_left
            (fun acc (var, expr) ->
              (* copy from TSassign *)
              let e = generate_expr var_table str_table expr in
              let offset = get_offset var_table var.v_name in
              acc
              ++ movq (lab "$my_array") (reg rbx)
              ++ pushq (ind ~ofs:offset rbx)
              ++ pushq (imm 0)
              ++ e
              ++ movq (lab "$my_array") (reg rbx)
              ++ movq (reg rax) (ind ~ofs:offset rbx))
            nop paired_list
          ++ call fn.fn_name
          ++ List.fold_left
               (fun acc var ->
                 let offset = get_offset var_table var.v_name in
                 acc
                 ++ movq (lab "$my_array") (reg rbx)
                 ++ popq rcx ++ popq rcx
                 ++ movq (reg rcx) (ind ~ofs:offset rbx))
               nop fn.fn_params)
  | TElist exprs ->
      movq (imm (16 + (List.length exprs * 8))) (reg rdi)
      ++ call "my_malloc"
      ++ movq (imm 4) (ind ~ofs:0 rax)
      ++ movq (imm (List.length exprs)) (ind ~ofs:8 rax)
      ++ pushq (reg rax)
      ++ (let s, i =
            List.fold_left
              (fun (stmt_acc, ind_acc) expr ->
                ( stmt_acc
                  ++ generate_expr var_table str_table expr
                  ++ popq rbx
                  ++ movq (reg rax) (ind ~ofs:ind_acc rbx)
                  ++ pushq (reg rbx),
                  ind_acc + 8 ))
              (nop, 16) exprs
          in
          s)
      ++ popq rax
  | TEget (e1, e2) ->
      generate_expr var_table str_table e1
      ++ pushq (reg rax)
      ++ generate_expr var_table str_table e2
      ++ pushq (reg rax)
      ++ popq r14
      ++ movq (ind ~ofs:8 r14) (reg r14)
      ++ popq r15
      ++ movq (ind ~ofs:16 ~index:r14 ~scale:8 r15) (reg rax)
  | _ -> nop

let my_print =
  let label_int = Printf.sprintf "print_int_end_%d" (get_label_counter ()) in
  let label_bool = Printf.sprintf "print_bool_end_%d" (get_label_counter ()) in
  let label_false =
    Printf.sprintf "print_false_end_%d" (get_label_counter ())
  in
  let label_None = Printf.sprintf "print_none_end_%d" (get_label_counter ()) in
  let label_str = Printf.sprintf "print_str_end_%d" (get_label_counter ()) in
  let label_list = Printf.sprintf "print_list_end_%d" (get_label_counter ()) in
  let label_print_end = Printf.sprintf "print_end_%d" (get_label_counter ()) in
  label "my_print"
  ++ pushq (reg rbp)
  ++ movq (reg rsp) (reg rbp)
  ++ movq (reg rdi) (reg rbx)
  ++ movq (ind ~ofs:0 rbx) (reg rax)
  (* print None *)
  ++ cmpq (imm 0) (reg rax)
  ++ jne label_None
  ++ movq (lab "$None") (reg rdi)
  ++ movq (imm 0) (reg rax)
  ++ call "printf" ++ jmp label_print_end ++ label label_None
  ++ movq (ind ~ofs:8 rbx) (reg rsi)
  (* print bool *)
  ++ cmpq (imm 1) (reg rax)
  ++ jne label_bool
  ++ cmpq (imm 1) (reg rsi)
  ++ jne label_false
  ++ movq (lab "$True") (reg rdi)
  ++ movq (imm 0) (reg rax)
  ++ call "printf" ++ jmp label_print_end ++ label label_false
  ++ movq (lab "$False") (reg rdi)
  ++ movq (imm 0) (reg rax)
  ++ call "printf" ++ jmp label_print_end ++ label label_bool
  (* print int *)
  ++ cmpq (imm 2) (reg rax)
  ++ jne label_int
  ++ movq (ind ~ofs:8 rbx) (reg rsi)
  ++ movq (lab "$format_int") (reg rdi)
  ++ movq (imm 0) (reg rax)
  ++ call "printf" ++ jmp label_print_end ++ label label_int
  (* print string *)
  ++ cmpq (imm 3) (reg rax)
  ++ jne label_str
  ++ movq (lab "$format_string") (reg rdi)
  ++ leaq (ind ~ofs:16 rbx) rsi
  ++ movq (imm 0) (reg rax)
  ++ call "printf" ++ jmp label_print_end ++ label label_str
  (* print list *)
  ++ cmpq (imm 4) (reg rax)
  ++ jne label_list
  ++ pushq (reg rbx)
  ++ pushq (imm 0)
  ++ movq (lab "$list_start") (reg rdi)
  ++ movq (imm 0) (reg rax)
  ++ call "printf"
  ++ (let loop_start = Printf.sprintf "loop_start_%d" (get_label_counter ()) in
      let loop_end = Printf.sprintf "loop_end_%d" (get_label_counter ()) in
      label loop_start ++ popq rax ++ popq rbx
      ++ cmpq (ind ~ofs:8 rbx) (reg rax)
      ++ je loop_end
      ++ movq (ind ~ofs:16 ~index:rax ~scale:8 rbx) (reg rdi)
      ++ incq (reg rax)
      ++ pushq (reg rbx)
      ++ pushq (reg rax)
      ++ call "my_print" ++ popq rax ++ popq rbx
      ++ cmpq (ind ~ofs:8 rbx) (reg rax)
      ++ je loop_end
      ++ pushq (reg rbx)
      ++ pushq (reg rax)
      ++ movq (lab "$comma") (reg rdi)
      ++ movq (imm 0) (reg rax)
      ++ call "printf"
      ++ movq (lab "$space") (reg rdi)
      ++ movq (imm 0) (reg rax)
      ++ call "printf" ++ jmp loop_start ++ label loop_end)
  ++ movq (lab "$list_end") (reg rdi)
  ++ movq (imm 0) (reg rax)
  ++ call "printf" ++ jmp label_print_end ++ label label_list
  ++ label label_print_end
  ++ movq (reg rbp) (reg rsp)
  ++ popq rbp
  ++ movq (imm 0) (reg rax)
  ++ ret

let rec generate_stmt var_table str_table = function
  | TSif (expr, stmt1, stmt2) ->
      let e = generate_expr var_table str_table expr in
      let cond_label = Printf.sprintf "cond_%d" (get_label_counter ()) in
      let then_label = Printf.sprintf "then_%d" (get_label_counter ()) in
      let else_label = Printf.sprintf "else_%d" (get_label_counter ()) in
      let end_label = Printf.sprintf "end_%d" (get_label_counter ()) in
      e
      ++ cmpq (imm 0) (ind ~ofs:8 rax)
      ++ je else_label ++ label then_label
      ++ generate_stmt var_table str_table stmt1
      ++ jmp end_label ++ label else_label
      ++ generate_stmt var_table str_table stmt2
      ++ jmp end_label ++ label end_label
  | TSreturn e -> generate_expr var_table str_table e ++ popq rbp ++ ret
  | TSassign (var, expr) ->
      let e = generate_expr var_table str_table expr in
      let offset =
        if Hashtbl.mem var_table var.v_name then
          Hashtbl.find var_table var.v_name
        else
          let offset = get_var_counter () * 8 in
          Hashtbl.add var_table var.v_name offset;
          offset
      in
      e
      ++ movq (lab "$my_array") (reg rbx)
      ++ movq (reg rax) (ind ~ofs:offset rbx)
  | TSprint expr ->
      generate_expr var_table str_table expr
      ++ movq (reg rax) (reg rdi)
      ++ call "my_print"
      ++ movq (lab "$new_line") (reg rdi)
      ++ movq (imm 0) (reg rax)
      ++ call "printf"
  | TSblock stmts ->
      List.fold_left
        (fun acc stmt -> acc ++ generate_stmt var_table str_table stmt)
        nop stmts
  | TSfor (var, expr, stmt) -> nop
  | TSeval e -> generate_expr var_table str_table e
  | TSset (e1, e2, e3) ->
      generate_expr var_table str_table e1
      ++ pushq (reg rax)
      ++ generate_expr var_table str_table e2
      ++ pushq (reg rax)
      ++ generate_expr var_table str_table e3
      ++ pushq (reg rax)
      (* align *)
      ++ pushq (imm 0)
      ++ popq r13 ++ popq r13 ++ popq r14
      ++ movq (ind ~ofs:8 r14) (reg r14)
      ++ popq r15
      ++ movq (reg r13) (ind ~ofs:16 ~index:r14 ~scale:8 r15)

let generate_def var_table str_table def =
  let f, stmt = def in
  label f.fn_name
  ++ pushq (reg rbp)
  ++ movq (reg rsp) (reg rbp)
  ++ generate_stmt var_table str_table stmt
  ++ generate_expr var_table str_table (TEcst Cnone)
  ++ movq (reg rbp) (reg rsp)
  ++ popq rbp
  ++ (if f.fn_name = "main" then movq (imm 0) (reg rax) else nop)
  ++ ret

let rec generate_text_section var_table str_table = function
  | [] -> nop
  | def :: tl ->
      let f, stmt = def in
      let e = generate_def var_table str_table def in
      e ++ generate_text_section var_table str_table tl

let generate_data_section str_table =
  label "format_int" ++ string "%d" ++ label "format_string" ++ string "%s"
  ++ label "True" ++ string "True" ++ label "False" ++ string "False"
  ++ label "None" ++ string "None" ++ label "new_line" ++ string "\n"
  ++ label "comma" ++ string "," ++ label "space" ++ string " "
  ++ label "list_start" ++ string "[" ++ label "list_end" ++ string "]"
  ++ label "my_array" ++ space 80000
  ++ Hashtbl.fold
       (fun key value acc -> acc ++ label value ++ string key)
       str_table nop

let my_malloc =
  label "my_malloc"
  ++ pushq (reg rbp)
  ++ movq (reg rsp) (reg rbp)
  ++ andq (imm (-16)) (reg rsp)
  ++ call "malloc"
  ++ movq (reg rbp) (reg rsp)
  ++ popq rbp ++ ret

let my_strcpy =
  label "my_strcpy"
  ++ pushq (reg rbp)
  ++ movq (reg rsp) (reg rbp)
  ++ andq (imm (-16)) (reg rsp)
  ++ call "strcpy"
  ++ movq (reg rbp) (reg rsp)
  ++ popq rbp ++ ret

let my_strcat =
  label "my_strcat"
  ++ pushq (reg rbp)
  ++ movq (reg rsp) (reg rbp)
  ++ andq (imm (-16)) (reg rsp)
  ++ call "strcat"
  ++ movq (reg rbp) (reg rsp)
  ++ popq rbp ++ ret

let util_function = my_malloc ++ my_print ++ my_strcpy ++ my_strcat

let file ?debug:(b = false) (p : Ast.tfile) : X86_64.program =
  debug := b;
  let text = generate_text_section global_var_table global_str_table p in
  let data = generate_data_section global_str_table in
  {
    text = globl "main" ++ text ++ util_function;
    (* TODO *)
    data;
  }
(* TODO *)
