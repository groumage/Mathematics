type attribut = Alg of string
							| Href of string
							| Type of string
							| Src of string
							| Id of string
							| Rows of int
							| Cols of int
							| Width of int
							| Height of int
							
type attributs = attribut list

type border_type = Solid
								 | Dotted

type style = Color of string
					 | FontSize of int
					 | BackgroundColor of string
					 | Border of int * border_type * string
	
type balise = Html
						| Title
						| Head
						| Body
						| H of int
						| B
						| I
						| U
						| Del
						| Ul
						| Li
						| P
						| Div
						| Script
						| Textarea
						| Button
						| Canvas
						| A
						| Empty
						| Br

type arbre =
	| Text of string
	| Balise of balise * attributs * arbre list

type document_html = {nom: string; document: arbre}

module B = struct
	type t = balise
	let compare = Pervasives.compare
end

module M = Map.Make(B)

type css = attributs M.t

let m = M.empty
let m = M.add (H 1) ([Color "blue"]) m
let m = M.add (H 2) ([Color "navy"]) m
let m = M.add (Canvas) ([Border (1, Solid, "#000000")]) m

let borderstyle_to_string b =
	match b with
		Solid -> "solid"
	| Dotted -> "dotted"

let style_to_string2 s =
	match s with
	  Color s -> "color : " ^ s ^ ";"
	| FontSize i -> "font-size : " ^ string_of_int i ^ "px;"
	| BackgroundColor s -> "background-color : " ^ s ^ ";"
	| Border (i, b, s) -> "border : " ^ string_of_int i ^ "px " ^ borderstyle_to_string b ^ " " ^ s ^ ";"

let rec tree_to_string tree name =
	match tree with
	  Text s -> s
	| Balise (b, a, t) -> (match b with
												   Html -> "<html " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</html>"
												 | Title -> "<title " ^ attributes_to_string a ^ ">\n" ^ name ^ tree_to_list t name ^ "</title>\n"
												 | Head -> "<head " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</head>\n"
												 | Body -> "<body " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</body>\n"
												 | H i -> "<h" ^ string_of_int i ^ " " ^ attributes_to_string a ^ "style = \"" ^ style_to_string (H i) ^ "\">\n" ^ tree_to_list t name ^ "</h" ^ string_of_int i ^ ">\n"
												 | B -> "<b " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</b>\n"
												 | I -> "<i " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</i>\n"
												 | U -> "<u " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</u>\n"
												 | Del -> "<del " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</del>\n"
												 | Ul -> "<ul " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</ul>\n"
												 | P -> "<p " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</p>\n"
												 | Li -> "<li " ^ attributes_to_string a ^ "style = \"" ^ style_to_string Li ^ "\">\n" ^ tree_to_list t name ^ "</li>\n"
												 | Script -> "<script " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</script>\n"
												 | Div -> "<div " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</div>\n"
												 | Textarea -> "<textarea " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</textarea>\n"
												 | Button -> "<button " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</button>\n"
												 | Canvas -> "<canvas " ^ attributes_to_string a ^ "style = \"" ^ style_to_string Canvas ^ "\">\n" ^ tree_to_list t name ^ "</canvas>\n"
												 | A -> "<a " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</a>\n"
												 | Empty -> attributes_to_string a ^ tree_to_list t name
												 | Br -> "<br/>\n" ^ attributes_to_string a ^ tree_to_list t name
												)
and tree_to_list tree_list name =
	match tree_list with
		[] -> ""
	| h :: t -> tree_to_string h name ^ tree_to_list t name
and attributes_to_string lst =
	match lst with
	  [] -> ""
	| h :: t -> match h with
								Alg s -> "align = \"" ^ s ^ "\" " ^ attributes_to_string t
							| Href s -> "href = \"" ^ s ^ "\" " ^ attributes_to_string t
							| Type s -> "type = \"" ^ s ^ "\" " ^ attributes_to_string t
							| Src s -> "src = \"" ^ s ^ "\" " ^ attributes_to_string t
							| Id s -> "id = \"" ^ s ^ "\" " ^ attributes_to_string t
							| Rows i -> "rows = \"" ^ string_of_int i ^ "\" " ^ attributes_to_string t
							| Cols i -> "cols = \"" ^ string_of_int i ^ "\" " ^ attributes_to_string t
							| Height i -> "height = \"" ^ string_of_int i ^ "\" " ^ attributes_to_string t
							| Width i -> "width = \"" ^ string_of_int i ^ "\" " ^ attributes_to_string t
and style_to_string balise =
	let a = M.find balise m in
	let rec decompose_lst lst =
		match lst with
			[] -> ""
		| h :: t -> style_to_string2 h ^ " " ^ decompose_lst t
	in
	decompose_lst a

let init_site =
	let page_name = ref "Mathemataume - Index" in
	let file_name = ref "index_web" in

	let index_title = Balise (Title, [], []) in
	let index_script = Balise (Script, [Type "text/javascript"; Src (!file_name ^ ".js")], []) in
	let index_head = Balise (Head, [], [index_title; index_script]) in
	let index_h1 = Balise (H 1, [Alg "center"], [Text "Mathemataume"]) in
	let index_ref_matrix = Balise (A, [Href "matrix_web.html"], [Text "Go to matrix"]) in
	let index_br = Balise (Br, [], []) in
	let index_ref_simp_expr = Balise (A, [Href "simp_expr_web.html"], [Text "Go to simplify expressions"]) in
	let index_ref_draw_function = Balise (A, [Href "draw_function_web.html"], [Text "Go to draw function"]) in
	let index_p2 = Balise (H 1, [], [Text "Welcome in Mathemataume's project !"]) in
	let index_body = Balise (Body, [], [index_h1; index_ref_matrix; index_br; index_ref_simp_expr; index_br; index_ref_draw_function; index_p2]) in
	let index_html = Balise (Html, [], [index_head; index_body]) in
	let index_page = { nom = !page_name; document = index_html } in

	let fic = ref (open_out (!file_name ^ ".html")) in
	output_string !fic (tree_to_string index_page.document index_page.nom);
	close_out !fic;

	page_name := "Mathemataume - Matrix";
	file_name := "matrix_web";

	let matrix_title = Balise (Title, [], []) in
	let matrix_script = Balise (Script, [Type "text/javascript"; Src (!file_name ^ ".js")], []) in
	let matrix_head = Balise (Head, [], [matrix_title; matrix_script]) in
	let matrix_h1 = Balise (H 1, [Alg "center"], [Text "Mathemataume"]) in
	let matrix_ref_index = Balise (A, [Href "index_web.html"], [Text "Go to index"]) in
	let matrix_br = Balise (Br, [], []) in
	let matrix_ref_simp_expr = Balise (A, [Href "simp_expr_web.html"], [Text "Go to simplify expressions"]) in
	let matrix_ref_draw_function = Balise (A, [Href "draw_function_web.html"], [Text "Go to draw function"]) in
	let matrix_p1 = Balise (H 2, [], [Text "Matrix"]) in
	let matrix_input_matrix = Balise (Textarea, [Id "input_matrix"; Rows 10; Cols 40], []) in
	let matrix_btn_calculate = Balise (Button, [Id "btn_calculate"], []) in
	let matrix_matrix_caracteristics = Balise (Div, [Id "matrix_caracteristics"], [Text "Dimensions: -<br/>"; Text "Determinant = -"]) in
	let matrix_body = Balise (Body, [], [matrix_h1; matrix_ref_index; matrix_br; matrix_ref_simp_expr; matrix_br; matrix_ref_draw_function; matrix_p1; matrix_input_matrix; matrix_btn_calculate; matrix_matrix_caracteristics]) in
	let matrix_html = Balise (Html, [], [matrix_head; matrix_body]) in
	let matrix_page = { nom = !page_name; document = matrix_html } in

	fic := open_out (!file_name ^ ".html");
	output_string !fic (tree_to_string matrix_page.document matrix_page.nom);
	close_out !fic;

	page_name := "Mathemataume - Simplify expression";
	file_name := "simp_expr_web";

	let simp_expr_title = Balise (Title, [], []) in
	let simp_expr_script = Balise (Script, [Type "text/javascript"; Src (!file_name ^ ".js")], []) in
	let simp_expr_head = Balise (Head, [], [simp_expr_title; simp_expr_script]) in
	let simp_expr_h1 = Balise (H 1, [Alg "center"], [Text "Mathemataume"]) in
	let simp_expr_ref_index = Balise (A, [Href "index_web.html"], [Text "Go to index"]) in
	let simp_expr_br = Balise (Br, [], []) in
	let simp_expr_ref_matrix = Balise (A, [Href "matrix_web.html"], [Text "Go to matrix"]) in
	let simp_expr_ref_draw_function = Balise (A, [Href "draw_function_web.html"], [Text "Go to draw function"]) in
	let simp_expr_p1 = Balise (H 2, [], [Text "Simplify expressions"]) in
	let simp_expr_input_expr = Balise (Textarea, [Id "input_expr"; Rows 2; Cols 40], []) in
	let simp_expr_btn_simplify = Balise (Button, [Id "btn_simplify"], []) in
	let simp_expr_output_expr = Balise (Textarea, [Id "output_expr"; Rows 2; Cols 40], []) in
	let simp_expr_info_expr_simp = Balise (Div, [Id "info_simp_expr"], []) in
	let simp_expr_body = Balise (Body, [], [simp_expr_h1; simp_expr_ref_index; simp_expr_br; simp_expr_ref_matrix; simp_expr_br; simp_expr_ref_draw_function; simp_expr_p1; simp_expr_input_expr; simp_expr_btn_simplify; simp_expr_output_expr; simp_expr_info_expr_simp]) in
	let simp_expr_html = Balise (Html, [], [simp_expr_head; simp_expr_body]) in
	let simp_expr_page = { nom = !page_name; document = simp_expr_html } in

	fic := open_out (!file_name ^ ".html");
	output_string !fic (tree_to_string simp_expr_page.document simp_expr_page.nom);
	close_out !fic;

	page_name := "Mathemataume - Draw function";
	file_name := "draw_function_web";

	let draw_function_title = Balise (Title, [], []) in
	let draw_function_script = Balise (Script, [Type "text/javascript"; Src (!file_name ^ ".js")], []) in
	let draw_function_head = Balise (Head, [], [draw_function_title; draw_function_script]) in
	let draw_function_h1 = Balise (H 1, [Alg "center"], [Text "Mathemataume"]) in
	let draw_function_ref_index = Balise (A, [Href "index_web.html"], [Text "Go to index"]) in
	let draw_function_br = Balise (Br, [], []) in
	let draw_function_ref_matrix = Balise (A, [Href "matrix_web.html"], [Text "Go to matrix"]) in
	let draw_function_ref_simp_expr = Balise (A, [Href "simp_expr_web.html"], [Text "Go to simplify expressions"]) in
	let draw_function_p2 = Balise (H 2, [], [Text "Draw function"]) in
	let draw_function_input_fct = Balise (Textarea, [Id "input_fct"; Rows 2; Cols 40], []) in
	let draw_function_btn_draw = Balise (Button, [Id "btn_draw"], []) in
	let draw_function_canvas = Balise (Canvas, [Id "canvas"; Width 320; Height 240], []) in
	let draw_function_div_canvas = Balise (Div, [Id "div_canvas"], [draw_function_canvas]) in
	let draw_function_body = Balise (Body, [], [draw_function_h1; draw_function_ref_index; draw_function_br; draw_function_ref_matrix; draw_function_br; draw_function_ref_simp_expr; draw_function_p2; draw_function_input_fct; draw_function_btn_draw; draw_function_div_canvas]) in
	let draw_function_html = Balise (Html, [], [draw_function_head; draw_function_body]) in
	let draw_function_page = { nom = !page_name; document = draw_function_html } in

	fic := open_out (!file_name ^ ".html");
	output_string !fic (tree_to_string draw_function_page.document draw_function_page.nom);
	close_out !fic