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

let draw_function () =
  let btn_draw = get_elem "btn_draw" in
  btn_draw##textContent <- Js.some (Js.string "Draw");
  let input_fct = get_textarea "input_fct" in
  input_fct##placeholder <- Js.string "Write your function ! ";
  let canvas = get_canvas "canvas" in
  let canvas2d = canvas##getContext (Dom_html._2d_) in
  canvas2d##fillStyle <- Js.string "rgb(200,200,200)";
  canvas2d##fillRect (0., 0., 320., 240.);
  canvas2d##fillStyle <- Js.string "rgb(255,0,0)";
  canvas2d##fillRect (40., 40., 15., 15.)

let onload _ = draw_function (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload