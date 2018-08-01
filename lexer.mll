{
	open Parser
	exception Eof
}

rule token = parse
  | [' ' '\t']          { token lexbuf }
  | ['\n']              { EOL }
  | ['0'-'9']+ as lxm   { FLOAT (float_of_string lxm) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIV }
  | '('                 { LPAR }
  | ')'                 { RPAR }
  | '^'                 { PUIS }
  | "cos"               { COS }
  | "sin"               { SIN }
  | "sqrt"              { SQRT }
  | "ln"                { LN }
  | "exp"               { EXP }
  | ['a'-'z']+ as lxm   { VAR (lxm) }
  | eof                 { raise Eof }