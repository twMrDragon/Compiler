open Ast

let debug = ref false
let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)
let global_var = Hashtbl.create 16
let function_table = Hashtbl.create 10

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc = dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(* 檢查特定 expression *)
let rec check_expr var_table = function
  | Ecst c -> TEcst c
  | Eident id ->
      if Hashtbl.mem var_table id.id then TEvar { v_name = id.id; v_ofs = 0 }
      else error ~loc:id.loc "variable %s is not defined" id.id
  | Ebinop (op, e1, e2) -> (
      let e1' = check_expr var_table e1 in
      let e2' = check_expr var_table e2 in
      match (e1', e2') with _, _ -> TEbinop (op, e1', e2'))
  | Eunop (op, e) -> TEunop (op, check_expr var_table e)
  | Ecall (id, args) -> (
      match id.id with
      | "len" ->
          if List.length args <> 1 then
            error ~loc:id.loc "len expects 1 argument but got %d"
              (List.length args)
          else
            TEcall
              ( { fn_name = id.id; fn_params = [] },
                [ check_expr var_table (List.hd args) ] )
      | "list" -> (
          match args with
          | [ Ecall (id2, args2) ] when id2.id = "range" -> (
              match args2 with
              | [ Ecst e ] -> (
                  match e with
                  | Cint n ->
                      let rec gen_list i =
                        match i with
                        | -1L -> []
                        | _ -> gen_list (Int64.sub i 1L) @ [ TEcst (Cint i) ]
                      in
                      TElist (gen_list (Int64.sub n 1L))
                  | _ ->
                      error ~loc:id2.loc "(typing) range takes 'int' argument")
              | [ e ] when List.length args2 = 1 ->
                  TErange (check_expr var_table e)
              | _ ->
                  error ~loc:id2.loc
                    "(typing) range expected exactly 1 argument(s), got %i"
                    (List.length args2))
          | _ ->
              error ~loc:id.loc
                "(typing) range expected exactly 1 argument(s), got %i"
                (List.length args))
      | "range" -> error ~loc:id.loc "range only used as an argument to list"
      | _ ->
          if not (Hashtbl.mem function_table id.id) then
            error ~loc:id.loc "function %s is not defined" id.id
          else if
            List.length args
            <> List.length (Hashtbl.find function_table id.id).fn_params
          then
            error ~loc:id.loc "function %s expects %d arguments but got %d"
              id.id
              (List.length (Hashtbl.find function_table id.id).fn_params)
              (List.length args)
          else
            let fn = Hashtbl.find function_table id.id in
            let targs = List.map (check_expr var_table) args in
            TEcall (fn, targs))
  | Elist es -> TElist (List.map (check_expr var_table) es)
  | Eget (e1, e2) -> TEget (check_expr var_table e1, check_expr var_table e2)

(* 檢查特定 statement *)
let rec check_stmt var_table = function
  | Sif (e, s1, s2) ->
      TSif
        ( check_expr var_table e,
          check_stmt var_table s1,
          check_stmt var_table s2 )
  | Sreturn e -> TSreturn (check_expr var_table e)
  | Sassign (id, e) ->
      let new_var = { v_name = id.id; v_ofs = 0 } in
      Hashtbl.add var_table id.id new_var;
      TSassign (new_var, check_expr var_table e)
  | Sprint e -> TSprint (check_expr var_table e)
  | Sblock stmts -> TSblock (List.map (check_stmt var_table) stmts)
  | Sfor (id, e, s) ->
      let new_var = { v_name = id.id; v_ofs = 0 } in
      Hashtbl.add var_table id.id new_var;
      TSfor (new_var, check_expr var_table e, check_stmt var_table s)
  | Seval e -> TSeval (check_expr var_table e)
  | Sset (e1, e2, e3) ->
      TSset
        ( check_expr var_table e1,
          check_expr var_table e2,
          check_expr var_table e3 )

(* 檢查特定 def *)
let rec check_def var_table def =
  let id, args, stmt = def in
  if
    List.mem id.id [ "len"; "list"; "range"; "print" ]
    || Hashtbl.mem function_table id.id
  then error ~loc:id.loc "function %s already defined" id.id
  else
    let local_var = Hashtbl.create 16 in
    Hashtbl.iter (fun k v -> Hashtbl.add local_var k v) var_table;
    List.iter
      (fun arg -> Hashtbl.add local_var arg.id { v_name = arg.id; v_ofs = 0 })
      args;
    let duplicates =
      List.find_opt
        (fun id -> List.length (List.filter (fun p -> p.id = id.id) args) > 1)
        args
    in
    match duplicates with
    | Some dup ->
        error ~loc:dup.loc
          "(typing) duplicate argument '%s' in function definition" dup.id
    | None ->
        let fn_args =
          List.map (fun arg -> { v_name = arg.id; v_ofs = 0 }) args
        in
        let fn = { fn_name = "def_" ^ id.id; fn_params = fn_args } in
        Hashtbl.add function_table id.id fn;
        (fn, check_stmt local_var stmt)

(* 檢查所有 def *)
let rec check_defs var_table defs =
  match defs with
  | [] -> []
  | def :: tl ->
      let cdef = check_def var_table def in
      cdef :: check_defs var_table tl

(* 檢查整個 file *)
let file ?debug:(b = false) (p : Ast.file) : Ast.tfile =
  debug := b;
  let def_list, main_stmt = p in
  let main_def = { fn_name = "main"; fn_params = [] } in
  (main_def, check_stmt global_var main_stmt) :: check_defs global_var def_list
