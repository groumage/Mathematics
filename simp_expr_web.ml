let doc = Dom_html.document

(* To log some debuging message in the browser console *)
let log s = Firebug.console##log (Js.string s)

let get_elem id =
    Js.Opt.get (doc##getElementById(Js.string id))
    (fun () -> log ("failed to find '" ^ id ^ "' element"); assert false)

let get_textarea id =
  let e = get_elem id in
  Js.Opt.get (Dom_html.CoerceTo.textarea e)
    (fun () -> log ("failed to find '" ^ id ^ "' textarea"); assert false)

let get_canvas id =
  let e = get_elem id in
  Js.Opt.get (Dom_html.CoerceTo.canvas e)
    (fun () -> log ("failed to find '" ^ id ^ "' canvas"); assert false)

let error_input_simplify_expression () =
  let div = get_elem "info_simp_expr" in
  div##innerHTML <- Js.string "This is not a correct expression"

let erase_error_input_simplify_expression () =
  let div = get_elem "info_simp_expr" in
  div##innerHTML <- Js.string ""

let eqn_of_string s  = Parser.yacc_eqn  Lexer.lexer_main (Lexing.from_string s)
let expr_of_string s = Parser.yacc_expr Lexer.lexer_main (Lexing.from_string s)

let parse_eqn  = eqn_of_string
let parse_expr = expr_of_string

let set_simplify_expression expr =
  let output_expr = get_textarea "output_expr" in
  let get_expr () =
    try
      let expr_simp = Function.string_of_expr (Function.simplify_expr (parse_expr (Js.to_string (Js.string expr)))) in
      output_expr##value <- Js.string expr_simp;
      erase_error_input_simplify_expression ()
    with 
      Parsing.Parse_error -> error_input_simplify_expression ()
  in
  get_expr ()

let simplify_expression () =
  let input_expr = get_textarea "input_expr" in
  input_expr##placeholder <- Js.string "Write your expression ...";
  input_expr##oninput <- Dom_html.handler (fun _ -> erase_error_input_simplify_expression (); Js._false);
  let output_expr = get_textarea "output_expr" in
  output_expr##placeholder <- Js.string "... and see the result !";
  let btn_simplify = get_elem "btn_simplify" in
  btn_simplify##textContent <- Js.some (Js.string "Simplify");
    let update_simplify_expression () =
      let expr = Js.to_string input_expr##value in
      if expr != "" then
        set_simplify_expression expr
      else
        error_input_simplify_expression ()
    in
  btn_simplify##onclick <- Dom_html.handler (fun ev -> update_simplify_expression (); Js._false)

let onload _ = simplify_expression (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload