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

let eqn_of_string s  = Parser.yacc_eqn  Lexer.lexer_main (Lexing.from_string s)
let expr_of_string s = Parser.yacc_expr Lexer.lexer_main (Lexing.from_string s)

let parse_eqn  = eqn_of_string
let parse_expr = expr_of_string

let set_matrix_caracteristics_with_det n p det =
  let div = get_elem "matrix_caracteristics" in
  if Util.is_float_int det then
    begin
      let carac = ref "" in
      carac := !carac ^ "Dimensions: " ^ string_of_int n ^ "x" ^ string_of_int p ^ "<br/>";
      carac := !carac ^ "Determinant = " ^ string_of_int (int_of_float det);
      div##innerHTML <- Js.string (!carac);
    end
  else
    begin
      let carac = ref "" in
      carac := !carac ^ "Dimensions: " ^ string_of_int n ^ "x" ^ string_of_int p ^ "<br/>";
      carac := !carac ^ "Determinant = " ^ string_of_float det;
      div##innerHTML <- Js.string (!carac)
    end

let set_matrix_caracteristics n p =
  let div = get_elem "matrix_caracteristics" in
    begin
      let carac = ref "" in
      carac := !carac ^ "Dimensions: " ^ string_of_int n ^ "x" ^ string_of_int p ^ "<br/>";
      carac := !carac ^ "Determinant = -";
      div##innerHTML <- Js.string (!carac);
    end

let error_input_matrix () =
  let div = get_elem "matrix_caracteristics" in
  div##innerHTML <- Js.string "This is not a matrix"

let erase_error_input_matrix () =
  let div = get_elem "matrix_caracteristics" in
  div##innerHTML <- Js.string "Dimensions: -<br/>Determinant = -"

let matrix () =
  let text1 = get_textarea "input_1" in
  text1##placeholder <- Js.string "Write your matrix !";
  text1##onchange <- erase_error_input_matrix ();
  let btn1 = get_elem "btn_1" in
  btn1##textContent <- Js.some (Js.string "Calculate");
    let update_matrix_info () =
      let string_matrix = Js.to_string text1##value in
      if Matrix.is_matrix string_matrix then
        let matrix = Matrix.get_matrix (string_matrix) in
        if Matrix.is_square_matrix matrix then
          set_matrix_caracteristics_with_det (Matrix.nb_lines matrix) (Matrix.nb_columns matrix) (Matrix.determinant matrix)
        else
          set_matrix_caracteristics (Matrix.nb_lines matrix) (Matrix.nb_columns matrix)
      else
        error_input_matrix ()
    in
  btn1##onclick <- Dom_html.handler (fun ev -> update_matrix_info (); Js._false)

let error_input_simplify_expression () =
  let div = get_elem "simplify_expr" in
  div##innerHTML <- Js.string "This is not a correct expression"

let erase_error_input_simplify_expression () =
  let div = get_elem "simplify_expr" in
  div##innerHTML <- Js.string ""

let set_simplify_expression expr =
  let text1 = get_textarea "output_1" in
  let get_expr () =
    try
      let expr_simp = Function.string_of_expr (Function.simplify_expr (parse_expr (Js.to_string (Js.string expr)))) in
      text1##value <- Js.string expr_simp;
      erase_error_input_simplify_expression ()
    with 
      Parsing.Parse_error -> error_input_simplify_expression ()
  in
  get_expr ()

let simplify_expression () =
  let text1 = get_textarea "input_2" in
  text1##placeholder <- Js.string "Write your expression ...";
  let text2 = get_textarea "output_1" in
  text2##placeholder <- Js.string "... and see the result !";
  let btn2 = get_elem "btn_2" in
  btn2##textContent <- Js.some (Js.string "Simplify");
    let update_simplify_expression () =
      let expression = Js.to_string text1##value in
      if expression != "" then
        set_simplify_expression expression
      else
        error_input_simplify_expression ()
    in
  btn2##onclick <- Dom_html.handler (fun ev -> update_simplify_expression (); Js._false)

let onload _ = matrix (); simplify_expression (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload