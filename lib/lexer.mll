{
open Parser
exception Error of string

(*关键字列表：将字符串关键字映射为对应的标记*)
let keywords = [
  ("int",      INT);
  ("void",     VOID);
  ("if",       IF);
  ("else",     ELSE);
  ("while",    WHILE);
  ("break",    BREAK);
  ("continue", CONTINUE);
  ("return",   RETURN);
]
}

(* 正则定义 *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | digit | '_')*  (*标识符定义*)

rule token = parse
  (*  Whitespace and Newlines  *)
  | [' ' '\t' '\r'] { token lexbuf }                (* 跳过空格、制表符和回车符 *)
  | '\n'            { Lexing.new_line lexbuf; token lexbuf } (* 记录行数 *)

  (*  Comments  *)
  | "//" [^ '\n']* { token lexbuf }                (* 单行注释 *)
  | "/*"           { comment lexbuf }               (* 多行注释 *)

  (*  整数 Literals *)
  | digit+ as num  { INT_LITERAL (int_of_string num) }

  (*  标识符和关键字 *)
  | ident as id    {
      try List.assoc id keywords
      with Not_found -> ID id
    }

  (* 运算符与符号 *)
  | "=="                     { EQ }
  | "!="                     { NEQ }
  | "<="                     { LE }
  | ">="                     { GE }
  | '<'                      { LT }
  | '>'                      { GT }

  | "&&"                     { AND }
  | "||"                     { OR }
  | '!'                      { NOT }

  | '='                      { ASSIGN }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { TIMES }
  | '/'                      { DIVIDE }
  | '%'                      { MOD }

  (* 分隔符 *)
  | '('                      { LPAREN }
  | ')'                      { RPAREN }
  | '{'                      { LBRACE }
  | '}'                      { RBRACE }
  | ';'                      { SEMI }
  | ','                      { COMMA }

  (* 文件结尾 *)
  | eof                      { EOF }

  (* 错误字符：遇到无法识别的字符 *)
   | _                        { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }  

and comment = parse
  | "*/"                     { token lexbuf }                    (* 结束多行注释 *)
  | eof                      { raise (Error "Unterminated comment") }  (*文件结束但注释未关闭*)
  | _                        { comment lexbuf }       (*继续处理注释内容*)
