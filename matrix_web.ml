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

let set_matrix_caracteristics n p =
  let div = get_elem "matrix_caracteristics" in
    begin
      let carac = ref "" in
      carac := !carac ^ "Dimensions: " ^ string_of_int n ^ "x" ^ string_of_int p ^ "<br/>";
      carac := !carac ^ "Determinant = -";
      div##innerHTML <- Js.string (!carac);
    end

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

let error_input_matrix () =
  let div = get_elem "matrix_caracteristics" in
  div##innerHTML <- Js.string "This is not a matrix"

let erase_error_input_matrix () =
  let div = get_elem "matrix_caracteristics" in
  div##innerHTML <- Js.string "Dimensions: -<br/>Determinant = -"

let matrix () =
  let input_matrix = get_textarea "input_matrix" in
  input_matrix##placeholder <- Js.string "Write your matrix !";
  input_matrix##oninput <- Dom_html.handler (fun _ -> erase_error_input_matrix (); Js._false);
  let btn_calculate = get_elem "btn_calculate" in
  btn_calculate##textContent <- Js.some (Js.string "Calculate");
    let update_matrix_info () =
      let string_matrix = Js.to_string input_matrix##value in
      if Matrix.is_matrix string_matrix then
        let matrix = Matrix.get_matrix (string_matrix) in
        if Matrix.is_square_matrix matrix then
          set_matrix_caracteristics_with_det (Matrix.nb_lines matrix) (Matrix.nb_columns matrix) (Matrix.determinant matrix)
        else
          set_matrix_caracteristics (Matrix.nb_lines matrix) (Matrix.nb_columns matrix)
      else
        error_input_matrix ()
    in
  btn_calculate##onclick <- Dom_html.handler (fun ev -> update_matrix_info (); Js._false)

let onload _ = matrix (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload