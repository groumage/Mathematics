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
												 | A -> "<a " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</a>"
												 | Empty -> attributes_to_string a ^ tree_to_list t name
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
	let index_p1 = Balise (H 2, [], [Text "Simplify expressions"]) in
	let index_input_expr = Balise (Textarea, [Id "input_expr"; Rows 2; Cols 40], []) in
	let index_btn_simplify = Balise (Button, [Id "btn_simplify"], []) in
	let index_output_expr = Balise (Textarea, [Id "output_expr"; Rows 2; Cols 40], []) in
	let index_info_expr_simp = Balise (Div, [Id "info_simp_expr"], []) in
	let index_p2 = Balise (H 2, [], [Text "Draw function"]) in
	let index_input_fct = Balise (Textarea, [Id "input_fct"; Rows 2; Cols 40], []) in
	let index_btn_draw = Balise (Button, [Id "btn_draw"], []) in
	let index_canvas = Balise (Canvas, [Id "canvas"; Width 320; Height 240], []) in
	let index_div_canvas = Balise (Div, [Id "div_canvas"], [index_canvas]) in
	let index_body = Balise (Body, [], [index_h1; index_ref_matrix; index_p1; index_input_expr; index_btn_simplify; index_output_expr; index_info_expr_simp; index_p2; index_input_fct; index_btn_draw; index_div_canvas]) in
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
	let matrix_p1 = Balise (H 2, [], [Text "Matrix"]) in
	let matrix_input_matrix = Balise (Textarea, [Id "input_matrix"; Rows 10; Cols 40], []) in
	let matrix_btn_calculate = Balise (Button, [Id "btn_calculate"], []) in
	let matrix_matrix_caracteristics = Balise (Div, [Id "matrix_caracteristics"], [Text "Dimensions: -<br/>"; Text "Determinant = -"]) in
	let matrix_body = Balise (Body, [], [matrix_h1; matrix_ref_index; matrix_p1; matrix_input_matrix; matrix_btn_calculate; matrix_matrix_caracteristics]) in
	let matrix_html = Balise (Html, [], [matrix_head; matrix_body]) in
	let matrix_page = { nom = !page_name; document = matrix_html } in

	fic := open_out (!file_name ^ ".html");
	output_string !fic (tree_to_string matrix_page.document matrix_page.nom);
	close_out !fic