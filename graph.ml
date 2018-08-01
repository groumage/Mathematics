type intervalle = {a: float; b: float}
type point = {x: float; y: float}
type ligne_polygonale = point array
type fenetre = {mutable abs: intervalle; mutable ord: intervalle}

let nombre_points = ref 200
let length_x = 900
let length_y = 700
let length_x_matrix = 400
let length_y_matrix = 400
let decalage_x = 100
let decalage_y = 100
let longueur_repere_visu = 3
let gray = Graphics.rgb 128 128 128
let max_abs = ref 15.
let min_abs = ref (-5.)
let max_ord = ref 15.
let min_ord = ref (-5.)

let decoupe_intervalle i n =
	let tab = Array.make (n+1) 0. in
	let pas = (i.b -. i.a) /. float_of_int n in
	for j = 0 to n do
		tab.(j) <- i.a +. float_of_int j *. pas;
	done;
tab

let longueur i =
	i.b -. i.a

let draw_frame_graph dim_x dim_y =
	Graphics.set_color Graphics.black;
	Graphics.draw_rect 0 (decalage_y) (dim_x - decalage_x) (dim_y - decalage_y - 1)

let draw_center_lines_graph dim_x dim_y x1 x2 y1 y2 =
	Graphics.set_color gray;
	let longueur_abscisse = dim_x - 2 - decalage_x in
	let longueur_ordonnee = dim_y - 2 - decalage_y in
	let nb_traits_abs = int_of_float (x2 -. x1) in
	let nb_traits_ord = int_of_float (y2 -. y1) in
	let pas_abs = longueur_abscisse / nb_traits_abs in
	let pas_ord = longueur_ordonnee / nb_traits_ord in
	if x1 <= 0. && 0. <= x2
	then
	begin
	Graphics.moveto ((int_of_float (Util.abs_float x1)) * pas_abs + 1) dim_y;
	Graphics.lineto ((int_of_float (Util.abs_float x1)) * pas_abs + 1) decalage_y
	end;
	if y1 <= 0. && 0. <= y2
	then
	begin
	Graphics.moveto 0 (dim_y - ((int_of_float (Util.abs_float y2)) * pas_ord + 1));
	Graphics.lineto (dim_x - decalage_x) (dim_y - ((int_of_float (Util.abs_float y2)) * pas_ord + 1))
	end;
	for i = 0 to nb_traits_ord do
		for j = 0 to nb_traits_abs do
			Graphics.moveto (1 + j * pas_abs) (dim_y - (1 + i * pas_ord - longueur_repere_visu));
			Graphics.lineto (1 + j * pas_abs) (dim_y - (1 + i * pas_ord + longueur_repere_visu));
			Graphics.moveto (1 + j * pas_abs - longueur_repere_visu) (dim_y - (1 + i * pas_ord));
			Graphics.lineto (1 + j * pas_abs + longueur_repere_visu) (dim_y - (1 + i * pas_ord))
		done
	done

let create_graph dim_x dim_y x1 x2 y1 y2 =
	let param = " " ^ string_of_int dim_x ^ "x" ^ string_of_int dim_y in
	Graphics.open_graph param;
	Graphics.set_window_title "Mathemataume Graphics";
	draw_frame_graph dim_x dim_y;
	draw_center_lines_graph dim_x dim_y x1 x2 y1 y2;
	let i1 = {a = x1; b = x2} in
	let i2 = {a = y1; b = y2} in
	{abs = i1; ord = i2}

let create_matrix m dim_x dim_y =
	let param = " " ^ string_of_int dim_x ^ "x" ^ string_of_int dim_y in
	Graphics.open_graph param;
	Graphics.set_window_title "Mathemataume Matrix";
	let n = Matrix.nb_lines m in
	let mem_pos_y = ref (Graphics.size_y() - 3) in
	let str_line = ref "" in
	for i = 0 to n-1 do
		str_line := Matrix.str_matrix_line m i;
		let (x_matrix, y_matrix) = Graphics.text_size !str_line in
		mem_pos_y := !mem_pos_y - y_matrix - 2;
		Graphics.moveto 5 !mem_pos_y;
		Graphics.draw_string !str_line
	done
	
let map f t =
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

let trace t c =
	Graphics.set_color c;
	let (x, y) = t.(0) in
	Graphics.moveto x y;
	for i = 0 to ((Array.length t) - 1) do
		let (x, y) = t.(i) in
		Graphics.lineto x y
	done;
	Graphics.set_color Graphics.white;
	Graphics.fill_rect 0 0 (Graphics.size_x()) (decalage_y - 1);
	Graphics.fill_rect (Graphics.size_x() - decalage_x) 0 decalage_x (Graphics.size_y())

let print_fct f =
	Graphics.set_color Graphics.black;
	let string_f = Function.string_fct f in
	let (x_text, y_text) = Graphics.text_size string_f in
	Graphics.moveto 5 (decalage_y - y_text - 5);
	Graphics.draw_string ("f(x) = " ^ string_f)

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
	((aux2 fen.abs (Graphics.size_x() - decalage_x) p.x), (aux2 fen.ord (Graphics.size_y() - decalage_y) p.y) + decalage_y)	
	
let en_tableau_pixel fen line_ply =
	let t = Array.make (Array.length line_ply) (0,0) in
	for i = 0 to ((Array.length line_ply) - 1) do
		t.(i) <- (conversion fen line_ply.(i))
	done;
t