open Ast
open Ir

(* --- 1. 环境与辅助函数 --- *)

type ir_scope = (string, string) Hashtbl.t  (* 原始名 -> 唯一名 *)

type env = {
  mutable temp_counter: int;  (* 用于生成新的临时变量 *)
  break_label: string option; (* 当前循环的结束标签 *)
  continue_label: string option; (* 当前循环的开始标签 *)
  mutable scopes: ir_scope list; (* 作用域栈，最顶层是当前作用域 *)
}

(* 全局 label 计数器，保证所有 label 唯一 *)
let global_label_counter = ref 0

(* 创建初始环境 *)
let make_env () = {
  temp_counter = 0;
  break_label = None;
  continue_label = None;
  scopes = [Hashtbl.create 32];
}

(* 生成一个新的临时变量操作数 *)
let new_temp env =
  let i = env.temp_counter in
  env.temp_counter <- i + 1;
  Temp i

(* 生成一个新的标签字符串 *)
let new_label _env =
  let i = !global_label_counter in
  global_label_counter := i + 1;
  "L" ^ string_of_int i

let enter_scope env =
  env.scopes <- (Hashtbl.create 32) :: env.scopes

let leave_scope env =
  match env.scopes with
  | _ :: tl -> env.scopes <- tl
  | [] -> failwith "作用域栈为空"

let add_var env id =
  let unique_name = id ^ "@" ^ string_of_int (List.length env.scopes) in
  Hashtbl.add (List.hd env.scopes) id unique_name

let find_var_unique env id =
  let rec find = function
    | [] -> None
    | tbl :: tl ->
        match Hashtbl.find_opt tbl id with
        | Some uname -> Some uname
        | None -> find tl
  in
  find env.scopes

(* --- 2. 表达式生成 --- *)

(*
 * gen_expr: 将 AST 表达式转换为 IR 指令序列
 * @param env - 当前环境
 * @param expr - 要转换的 AST 表达式
 * @return (operand, instr list) - 一个元组，包含:
 *   - 存放表达式结果的操作数 (可能是 Const, Temp, 或 Name)
 *   - 为计算该结果而生成的指令列表
 *)
let rec gen_expr env expr : operand * instr list =
  match expr with
  | IntLit n ->
      (* 常量直接返回 Const 操作数，不产生任何指令 *)
      (Const n, [])

  | Var id ->
      (match find_var_unique env id with
      | Some uname -> (Name uname, [])
      | None -> failwith ("IR生成阶段：未声明的变量 '" ^ id ^ "'"))

  | UnOp (op, e) ->
      let (e_op, e_instrs) = gen_expr env e in
      let dest_temp = new_temp env in
      let unop_instr = UnOp { dest = dest_temp; op = op; src = e_op } in
      (dest_temp, e_instrs @ [unop_instr])

  | BinOp (op, e1, e2) ->
      let (e1_op, e1_instrs) = gen_expr env e1 in
      let (e2_op, e2_instrs) = gen_expr env e2 in
      let dest_temp = new_temp env in
      let binop_instr = BinOp { dest = dest_temp; op = op; src1 = e1_op; src2 = e2_op } in
      (dest_temp, e1_instrs @ e2_instrs @ [binop_instr])

  | Call (fname, args) ->
      (* 依次计算所有参数，并收集它们的指令和结果操作数 *)
      let (arg_ops, arg_instrs_list) =
        List.map (gen_expr env) args |> List.split
      in
      let all_arg_instrs = List.flatten arg_instrs_list in

      (* 返回值需要存放在一个新的临时变量中 *)
      let dest_temp = new_temp env in
      let call_instr = Call { dest = Some dest_temp; name = fname; args = arg_ops } in
      (dest_temp, all_arg_instrs @ [call_instr])


(* --- 3. 语句生成 --- *)

(*
 * gen_stmt: 将 AST 语句转换为 IR 指令序列
 * @param env - 当前环境
 * @param stmt - 要转换的 AST 语句
 * @return instr list - 生成的指令列表
 *)
let rec gen_stmt env stmt : instr list =
  match stmt with
  | Block stmts ->
      (* 将块内所有语句生成的指令列表连接起来 *)
      enter_scope env;
      let instrs = List.map (gen_stmt env) stmts |> List.flatten in
      leave_scope env;
      instrs

  | Expr e ->
      (* 计算表达式，但忽略其结果。处理`func();` 这样的调用 *)
      let (_, instrs) = gen_expr env e in
      instrs

  | VarDecl (_, id, init_opt) ->
      add_var env id;
      let uname = match find_var_unique env id with Some u -> u | None -> assert false in
      (match init_opt with
      | Some init_expr ->
          let (e_op, e_instrs) = gen_expr env init_expr in
          let move_instr = Move { dest = Name uname; src = e_op } in
          e_instrs @ [move_instr]
      | None ->
          []
      )

  | Assign (id, e) ->
      (match find_var_unique env id with
      | Some uname ->
          let (e_op, e_instrs) = gen_expr env e in
          let move_instr = Move { dest = Name uname; src = e_op } in
          e_instrs @ [move_instr]
      | None -> failwith ("IR生成阶段：赋值给未声明的变量 '" ^ id ^ "'"))

  | If (cond, then_stmt, else_opt) ->
      let label_true = new_label env in
      let label_false = new_label env in
      let label_end = new_label env in

      let (cond_op, cond_instrs) = gen_expr env cond in
      let cjump_instr = CJump { cond = cond_op; label_true; label_false } in

      let then_instrs = gen_stmt env then_stmt in
      
      (match else_opt with
      | Some else_s ->
          let else_instrs = gen_stmt env else_s in
          cond_instrs
          @ [cjump_instr]
          @ [Label label_true] @ then_instrs @ [Jump label_end]
          @ [Label label_false] @ else_instrs
          @ [Label label_end]
      | None ->
          cond_instrs
          @ [cjump_instr]
          @ [Label label_true] @ then_instrs
          @ [Label label_false] (* 如果没有 else，false 分支就是 if 语句的结尾 *)
      )

  | While (cond, body) ->
    let label_start = new_label env in (* 循环开始/条件判断 *)
    let label_body = new_label env in  (* 循环体 *)
    let label_end = new_label env in   (* 循环结束 *)

    (* 创建一个包含循环上下文的新环境 *)
    let loop_env = { env with
      break_label = Some label_end;
      continue_label = Some label_start;
    } in

    let (cond_op, cond_instrs) = gen_expr loop_env cond in
    let cjump_instr = CJump { cond = cond_op; label_true = label_body; label_false = label_end } in
    
    (* 使用 loop_env 来生成循环体的指令 *)
    let body_instrs = gen_stmt loop_env body in

    [Label label_start]
    @ cond_instrs
    @ [cjump_instr]
    @ [Label label_body]
    @ body_instrs
    @ [Jump label_start] (* 循环回到开始处 *)
    @ [Label label_end]

  | Break ->
    (match env.break_label with
    | Some lbl -> [Jump lbl]
    | None -> failwith "语义分析阶段应已捕获此错误: break 不在循环内")

  | Continue ->
    (match env.continue_label with
    | Some lbl -> [Jump lbl]
    | None -> failwith "语义分析阶段应已捕获此错误: continue 不在循环内") 

  | Return e_opt ->
      (match e_opt with
      | Some e ->
          let (e_op, e_instrs) = gen_expr env e in
          e_instrs @ [Return (Some e_op)]
      | None -> [Return None]
      )

  | EmptyStmt -> []

(* 
(* --- 尾递归优化辅助 --- *)

(* 判断一个 IR 指令是否为对本函数的尾递归调用 *)
let is_tail_recursive_call func_name instr =
  match instr with
  | Call { dest; name; args } when name = func_name -> Some (dest, args)
  | _ -> None

(* 对函数体 IR 指令列表做尾递归优化（参数赋值用临时变量避免覆盖） *)
let tailcall_optimize_instrs func_name param_unames instrs =
  let rec aux acc = function
    | Call { dest; name; args } :: Return (Some ret_op) :: tl
      when name = func_name && (match dest with Some d -> d = ret_op | None -> false) ->
        (* 1. 用临时变量保存所有参数 *)
        let temp_names =
          List.mapi (fun i _ -> Temp (100000 + i)) param_unames
        in
        let save_tmps =
          List.map2 (fun t arg -> Move { dest = t; src = arg }) temp_names args
        in
        let restore_params =
          List.map2 (fun pname t -> Move { dest = Name pname; src = t }) param_unames temp_names
        in
        aux (List.rev restore_params @ (Jump "tailrec_entry") :: List.rev save_tmps @ acc) tl
    | Call { dest; name; args } :: Return None :: tl
      when name = func_name && dest = None ->
        let temp_names =
          List.mapi (fun i _ -> Temp (100000 + i)) param_unames
        in
        let save_tmps =
          List.map2 (fun t arg -> Move { dest = t; src = arg }) temp_names args
        in
        let restore_params =
          List.map2 (fun pname t -> Move { dest = Name pname; src = t }) param_unames temp_names
        in
        aux (List.rev restore_params @ (Jump "tailrec_entry") :: List.rev save_tmps @ acc) tl
    | hd :: tl -> aux (hd :: acc) tl
    | [] -> List.rev acc
  in
  aux [] instrs

(* --- 4. 顶层转换函数 --- *)

(* 将单个 AST 函数定义转换为 IR 函数定义 *)
let gen_func_def (fdef: Ast.func_def) : ir_func =
  let env = make_env () in
  List.iter (fun p -> add_var env p.pname) fdef.params;
  let param_unames =
    List.map (fun p -> match find_var_unique env p.pname with Some u -> u | None -> assert false) fdef.params
  in
  let body_instrs = gen_stmt env fdef.body in
  let optimized =
    tailcall_optimize_instrs fdef.fname param_unames body_instrs
  in
  let body_instrs =
    (Label "tailrec_entry") :: optimized
  in
  {
    name = fdef.fname;
    params = param_unames;
    body = body_instrs;
  }

(* 程序的总入口：将整个 AST 编译单元转换为 IR 程序 *)
let generate (prog: Ast.comp_unit) : ir_program =
  List.map gen_func_def prog *)

  (* --- 4. 顶层转换函数 --- *)

(* --- 尾递归优化 --- *)

(* 判断语句是否为尾递归调用 *)
let is_tail_recursive_call fname params stmt =
  match stmt with
  | Ast.Return (Some (Ast.Call (name, args))) when name = fname && List.length args = List.length params -> 
      (* 检查参数是否直接对应形式参数，避免不必要的拷贝 *)
      let rec check_direct_params args params =
        match (args, params) with
        | (Ast.Var a)::args_tl, p::params_tl when a = p.pname -> 
            check_direct_params args_tl params_tl
        | [], [] -> true
        | _ -> false
      in
      if check_direct_params args params then `DirectParams args
      else `NeedCopy args
  | _ -> `NotTailRec

(* 优化后的尾递归转换 *)
let transform_tailrec env fname params stmt =
  match is_tail_recursive_call fname params stmt with
  | `DirectParams args ->
      (* 情况1：参数直接对应形式参数，只需跳转 *)
      [Jump ("tailrec_entry_" ^ fname)]
      
  | `NeedCopy args ->
      (* 情况2：需要参数拷贝 *)
      let (arg_ops, arg_instrs) = 
        List.map (gen_expr env) args 
        |> List.split 
        |> fun (ops, instrs) -> (ops, List.flatten instrs)
      in
      let copies =
        List.map2 (fun param op ->
          Move { dest = Name (find_var_unique env param.pname |> Option.get); src = op }
        ) params arg_ops  (* 注意这里添加了闭合的括号 *)
      in
      arg_instrs @ copies @ [Jump ("tailrec_entry_" ^ fname)]
      
  | `NotTailRec ->
      gen_stmt env stmt

(* 优化后的函数定义生成 *)
let gen_func_def (fdef: Ast.func_def) : ir_func =
  let env = make_env () in
  List.iter (fun p -> add_var env p.pname) fdef.params;
  let param_unames =
    List.map (fun p -> find_var_unique env p.pname |> Option.get) fdef.params
  in
  let label_entry = "tailrec_entry_" ^ fdef.fname in
  
  (* 新的函数体生成逻辑 *)
  let rec gen_optimized_body = function
    | Ast.Block stmts ->
        let process_stmt stmt (acc, is_tail) =
          if is_tail then (acc, true) (* 尾位置之后的语句不会执行 *)
          else
            match stmt with
            | Ast.Return _ -> 
                (acc @ transform_tailrec env fdef.fname fdef.params stmt, true)
            | _ -> 
                (acc @ gen_stmt env stmt, false)
        in
        let (instrs, _) = List.fold_left process_stmt ([], false) stmts in
        [Label label_entry] @ instrs
        
    | other_stmt ->
        [Label label_entry] @ transform_tailrec env fdef.fname fdef.params other_stmt
  in
  
  {
    name = fdef.fname;
    params = param_unames;
    body = gen_optimized_body fdef.body;
  }

(* 程序的总入口：将整个 AST 编译单元转换为 IR 程序 *)
let generate (prog: Ast.comp_unit) : ir_program =
  List.map gen_func_def prog
