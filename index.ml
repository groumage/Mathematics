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
  div##innerHTML <- Js.string ("Dimensions: " ^ string_of_int n ^ "x" ^ string_of_int p ^ "<br/>Determinant = " ^ string_of_float det)

let error_input_matrix () =
  let div = get_elem "matrix_caracteristics" in
  div##innerHTML <- Js.string "This is not a matrix"

let matrix () =
  let text1 = get_textarea "input_1" in
  text1##placeholder <- Js.string "Write your matrix !";
  let btn1 = get_elem "btn_1" in
  btn1##textContent <- Js.some (Js.string "Calculate");

  let calcul () =
    let input = Js.to_string text1##value in
    let matrix = Matrix.get_matrix (input) in
    set_matrix_caracteristics 10 10 (Matrix.determinant matrix)
  in
  btn1##onclick <- Dom_html.handler (fun ev -> calcul(); Js._false)

 (* btn1##onclick <- Dom_html.handler (if Matrix.is_matrix (Js.to_string text1##value) 
                                     then (let matrix = Matrix.get_matrix (Js.to_string text1##value) in
                                      fun ev -> (set_matrix_caracteristics (Matrix.nb_lines matrix) (Matrix.nb_columns matrix) (Matrix.determinant matrix) ; Js._false))
                                     else fun ev -> (error_input_matrix() ; Js._false))
    if Matrix.is_matrix (Js.to_string text1##value) 
     then (let matrix = Matrix.get_matrix (Js.to_string text1##value) in
      fun ev -> (set_matrix_caracteristics (Matrix.nb_lines matrix) (Matrix.nb_columns matrix) (Matrix.determinant matrix) ; Js._false))
     else fun ev -> (error_input_matrix() ; Js._false)*)


let test2 () =
  let t1 = get_textarea "input" in
  t1##placeholder <- Js.string "What's your name ?";
  let t2 = get_textarea "result" in
  let b2 = get_elem "b-conv" in
  b2##textContent <- Js.some (Js.string "Go");
  let say_hello () =
    let input = Js.to_string t1##value in
    let matrix = Matrix.get_matrix (input) in
    t2##value <- Js.string ("Hello "^input^" !" ^ string_of_float (Matrix.determinant matrix))
  in
  b2##onclick <- Dom_html.handler (fun ev -> say_hello (); Js._false)

let simplify_expression () =
  let text1 = get_textarea "input_2" in
  let text2 = get_textarea "output_1" in
  let btn2 = get_elem "btn_2" in
  text1##placeholder <- Js.string "Write your expression ...";
  text2##placeholder <- Js.string "... and see the result !";
  btn2##textContent <- Js.some (Js.string "Simplify");
  btn2##onclick <- Dom_html.handler (fun ev -> text2##value <- text1##value; Js._false)
  (*btn2##onclick <- Dom_html.handler (fun ev -> text2##value <- Js.string "click !"; Js._false)*)
  (*let body = get_elem "test1" in
  let textbox = Dom_html.createTextarea doc in

  let button = Dom_html.createButton doc in
  button##textContent <- Js.some (Js.string "Say hello !");
  button##onclick <- Dom_html.handler (fun ev -> textbox##value <- Js.string "Hello !"; Js._false);
  Dom.appendChild body textbox;
  let body = get_elem "test3" in
  Dom.appendChild body button*)

(*let test2 () =
  let t1 = get_textarea "input" in
  t1##placeholder <- Js.string "What's your name ?";
  let t2 = get_textarea "result" in
  let b2 = get_elem "b-conv" in
  b2##textContent <- Js.some (Js.string "Go");
  let say_hello () =
    let input = Js.to_string t1##value in
    t2##value <- Js.string ("Hello "^input^" !")
  in
  b2##onclick <- Dom_html.handler (fun ev -> say_hello (); Js._false)*)

let onload _ = matrix (); simplify_expression(); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload

(*let test1 () =
  let body = get_elem "test1" in
  let textbox = Dom_html.createTextarea doc in
  textbox##rows <- 2; textbox##cols <- 40;
  textbox##value <- Js.string "Nothing yet...";

  let button = Dom_html.createButton doc in
  button##textContent <- Js.some (Js.string "Say hello !");
  button##onclick <- Dom_html.handler (fun ev -> textbox##value <- Js.string "Hello !"; Js._false);
  Dom.appendChild body textbox;
  Dom.appendChild body button*)


(*let onload _ = test1 (); test2 (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload*)