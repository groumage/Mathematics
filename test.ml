
let doc = Dom_html.document

(* To log some debuging message in the browser console *)
let log s = Firebug.console##log (Js.string s)

let get_elem id =
    Js.Opt.get (doc##getElementById(Js.string id))
    (fun () -> log ("failed to find '"^id^"' element"); assert false)

let get_textarea id =
  let e = get_elem id in
  Js.Opt.get (Dom_html.CoerceTo.textarea e)
    (fun () -> log ("failed to find '"^id^"' textarea"); assert false)

let matrix () =
  let text1 = get_textarea "input_1" in
  let text2 = get_textarea "output_1" in
  let btn1 = get_elem "btn_1" in
  btn1##onclick <- Dom_html.handler (fun ev -> text2##value <- Js.string "click !"; Js._false)
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

let onload _ = matrix (); Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload