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

let eqn_of_string s  = Parser.yacc_eqn  Lexer.lexer_main (Lexing.from_string s)
let expr_of_string s = Parser.yacc_expr Lexer.lexer_main (Lexing.from_string s)

let parse_eqn  = eqn_of_string
let parse_expr = expr_of_string

(*let erase () =
  let canvas = get_canvas "canvas" in
  let canvas2d = canvas##getContext (Dom_html._2d_) in
    canvas2d##strokeStyle <- Js.string ("white");
    for i = 0 to canvas##height do
      canvas2d##moveTo (0., float_of_int i);
      canvas2d##lineTo (float_of_int canvas##width, float_of_int i);
    done;
    canvas2d##stroke ()*)

let draw_function () =
  let btn_draw = get_elem "btn_draw" in
  btn_draw##textContent <- Js.some (Js.string "Draw");
  let input_fct = get_textarea "input_fct" in
  input_fct##placeholder <- Js.string "Write your function ! ";
  let canvas = get_canvas "canvas" in
  let canvas2d = canvas##getContext (Dom_html._2d_) in
  let erase () =
    canvas2d##beginPath ();
    canvas2d##strokeStyle <- Js.string ("rgba(255,255,255,1)");
    canvas2d##clearRect (0., 0., float_of_int canvas##width, float_of_int canvas##height);
    canvas2d##strokeStyle <- Js.string ("rgba(0,0,255,1)");
    canvas2d##closePath ();
    canvas2d##stroke ()
  in
  input_fct##oninput <- Dom_html.handler (fun ev -> erase (); Js._false);
  let draw () =
    canvas2d##beginPath ();
    erase ();
    let expr = Js.to_string input_fct##value in
    let expr_simp = Function.simplify_expr (parse_expr (Js.to_string (Js.string expr))) in
    let zip_tab = Graph.get_zip_tab expr_simp in
    let (x0, y0) = zip_tab.(0) in
    canvas2d##moveTo (float_of_int x0, float_of_int canvas##height -. float_of_int y0);
    for i = 0 to ((Array.length zip_tab) - 1) do
      let (x, y) = zip_tab.(i) in
      canvas2d##lineTo (float_of_int x, float_of_int canvas##height -. float_of_int y)
    done;
    canvas2d##closePath();
    canvas2d##stroke ()
  in
  btn_draw##onclick <- Dom_html.handler (fun ev -> draw (); Js._false)

let onload _ = draw_function (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload