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

let set_matrix_caracteristics n p det =
  let div = get_elem "matrix_caracteristics" in
  if Util.is_float_int det then
    div##innerHTML <- Js.string ("Dimensions: " ^ string_of_int n ^ "x" ^ string_of_int p ^ "<br/>Determinant = " ^ string_of_int (int_of_float det))
  else 
    div##innerHTML <- Js.string ("Dimensions: " ^ string_of_int n ^ "x" ^ string_of_int p ^ "<br/>Determinant = " ^ string_of_float det)

let error_input_matrix () =
  let div = get_elem "matrix_caracteristics" in
  div##innerHTML <- Js.string "This is not a matrix"

let matrix () =
  let text1 = get_textarea "input_1" in
  text1##placeholder <- Js.string "Write your matrix !";
  let btn1 = get_elem "btn_1" in
  btn1##textContent <- Js.some (Js.string "Calculate");
    let update_matrix_info () =
      let string_matrix = Js.to_string text1##value in
      if Matrix.is_matrix string_matrix then
        let matrix = Matrix.get_matrix (string_matrix) in
        set_matrix_caracteristics (Matrix.nb_lines matrix) (Matrix.nb_columns matrix) (Matrix.determinant matrix)
      else
        error_input_matrix ()
    in
  btn1##onclick <- Dom_html.handler (fun ev -> update_matrix_info (); Js._false)

let simplify_expression () =
  let text1 = get_textarea "input_2" in
  let text2 = get_textarea "output_1" in
  let btn2 = get_elem "btn_2" in
  text1##placeholder <- Js.string "Write your expression ...";
  text2##placeholder <- Js.string "... and see the result !";
  btn2##textContent <- Js.some (Js.string "Simplify");
  btn2##onclick <- Dom_html.handler (fun ev -> text2##value <- text1##value; Js._false)

let onload _ = matrix (); simplify_expression (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload