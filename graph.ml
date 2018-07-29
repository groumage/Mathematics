type intervalle = {a: float; b: float}
type point = {x: float; y: float}
type ligne_polygonale = point array
type fenetre = {mutable abs: intervalle; mutable ord: intervalle}

let nombre_points = ref 500
let decalage_x = 100
let decalage_y = 100

let create_line_graph dim_x dim_y =
	Graphics.set_color Graphics.black;
	Graphics.draw_rect 0 (decalage_y - 1) (dim_x - decalage_x) (dim_y - decalage_y);
	Graphics.set_color (Graphics.rgb 128 128 128);
	Graphics.draw_rect 20 5 200 50

let create_center_line dim_x dim_y =
	Graphics.set_color (Graphics.rgb 128 128 128);
	Graphics.moveto 1 ((dim_y / 2) + (decalage_y / 2));
	Graphics.lineto ((dim_x - decalage_x) - 1) ((dim_y / 2) + (decalage_y / 2));
	Graphics.moveto ((dim_x / 2) - (decalage_x / 2)) (decalage_y - 1);
	Graphics.lineto ((dim_x / 2) - (decalage_x / 2)) (dim_y - 2)

let create_fenetre dim_x dim_y x1 x2 y1 y2 =
	let param = " " ^ string_of_int dim_x ^ "x" ^ string_of_int dim_y in
	Graphics.open_graph param;
	Graphics.set_window_title "(Gui)^2 Graphics";
	create_line_graph dim_x dim_y;
	create_center_line dim_x dim_y;
	let i1 = {a = x1; b = x2} in
	let i2 = {a = y1; b = y2} in
	{abs = i1; ord = i2}

let decoupe_intervalle i n =
	let tab = Array.make (n+1) 0. in
	let pas = (i.b -. i.a) /. float_of_int n in
	for j = 0 to n do
		tab.(j) <- i.a +. float_of_int j *. pas;
	done;
tab
	
let map f t =
	Graphics.set_color Graphics.black;
	let string_f = Function.string_fct f in
	let (x_text, y_text) = Graphics.text_size string_f in
	Graphics.moveto 5 (decalage_x - y_text - 5);
	Graphics.draw_string ("f(x) = " ^ string_f);
	let tab = Array.make (Array.length t) 0. in
	for i = 0 to ((Array.length t) - 1) do
		tab.(i) <- Function.calcul_fct f t.(i)
	done;
tab

let zip t1 t2 =
	if Array.length t1 != Array.length t2
	then
		failwith "zip: wrong tab dimensions"
	else
		let t = Array.make (Array.length t1) ({x = 0.; y = 0.}) in
		for i = 0 to ((Array.length t) - 1) do
			t.(i) <- {x = t1.(i); y = t2.(i)}
		done;
t

let is_in_fen x y =
	if x >= 1 && x <= Graphics.size_x() - decalage_x - 2 && y >= decalage_y + 1 && y <= Graphics.size_y() - 1
	then true
	else false

let trace t c =
	Graphics.set_color c;
	let indice = ref (-1) in
	for i = 0 to ((Array.length t) - 1) do
		let (x, y) = t.(i) in
		if is_in_fen x y && !indice == -1
		then
			begin
				Graphics.moveto x y;
				indice := i
			end
	done;
	for i = !indice to ((Array.length t) - 1) do
		let (x, y) = t.(i) in
		if is_in_fen x y
		then Graphics.lineto x y
		else Graphics.lineto x 99
	done

let longueur i =
	i.b -. i.a

let is_in_fen x y =
	if x <= (Graphics.size_x() - decalage_x) && y >= decalage_y
	then true
	else false

let proportion i x =
	(x -. i.a) /. longueur i

let aux p n =
	let res = p *. float_of_int n in
	let part_ent = int_of_float res in
	let decimale = Util.abs_float (res -. float_of_int part_ent) in
	if decimale < 0.5 then part_ent else part_ent + 1
		
let aux2 i n x =
	aux (proportion i x) n
	
let conversion fen p =
	((aux2 fen.abs (Graphics.size_x() - decalage_x - 2) p.x) + 1, (aux2 fen.ord (Graphics.size_y() - decalage_y) p.y) + decalage_y)
	
let en_tableau_pixel fen line_ply =
	let t = Array.make (Array.length line_ply) (0,0) in
	for i = 0 to ((Array.length line_ply) - 1) do
		t.(i) <- (conversion fen line_ply.(i))
	done;
t