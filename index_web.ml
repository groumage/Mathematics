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

let onload _ = Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload