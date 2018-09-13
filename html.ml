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
						| Link
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
												 | Link -> "<link " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t name ^ "</link>\n"
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
	let page_name = ref "index" in

	let index_title = Balise (Title, [], []) in
	let index_script = Balise (Script, [Type "text/javascript"; Src (!page_name ^ ".js")], []) in
	let index_head = Balise (Head, [], [index_title; index_script]) in
	let index_h1 = Balise (H 1, [Alg "center"], [Text "Mathemataume"]) in
	let index_p1 = Balise (H 2, [], [Text "Matrix"]) in
	let index_text1 = Balise (Textarea, [Id "input_1"; Rows 10; Cols 40], []) in
	let index_btn1 = Balise (Button, [Id "btn_1"], []) in
	let index_matrix_caracteristics = Balise (Div, [Id "matrix_caracteristics"], [Text "Dimensions: -<br/>"; Text "Determinant = -"]) in
	let index_p2 = Balise (H 2, [], [Text "Simplify expressions"]) in
	let index_text2 = Balise (Textarea, [Id "input_2"; Rows 2; Cols 40], []) in
	let index_btn2 = Balise (Button, [Id "btn_2"], []) in
	let index_text3 = Balise (Textarea, [Id "output_1"; Rows 2; Cols 40], []) in
	let index_info_expr_simp = Balise (Div, [Id "simplify_expr"], []) in
	let index_p3 = Balise (H 2, [], [Text "Draw function"]) in
	let index_text4 = Balise (Textarea, [Id "input_3"; Rows 2; Cols 40], []) in
	let index_btn3 = Balise (Button, [Id "btn_3"], []) in
	let index_canvas = Balise (Canvas, [Id "graphic"; Width 320; Height 240], []) in
	let index_div_canvas = Balise (Div, [Id "canvas"], [index_canvas]) in
	let index_body = Balise (Body, [], [index_h1; index_p1; index_text1; index_btn1; index_matrix_caracteristics; index_p2; index_text2; index_btn2; index_text3; index_info_expr_simp; index_p3; index_text4; index_btn3; index_div_canvas]) in
	let index_html = Balise (Html, [], [index_head; index_body]) in
	let index_page = { nom = !page_name; document = index_html } in

	let fic = ref (open_out (index_page.nom ^ ".html")) in
	output_string !fic (tree_to_string index_page.document index_page.nom);
	close_out !fic
(*
	page_name := "matrix" in

	let matrix_title = Balise (Title, [], [Text "Mathemataume's project\n"]) in
	let matrix_html = Balise (Html, [], []) in

	let matrix_page = { nom = !page_name; document = matrix_html } in
	fic := open_out (matrix_page.nom ^ ".html") in
	output_string !fic (tree_to_string matrix_page.document);
	close_out !fic*)
(*
	let b7 = Balise (I, [], [Text ("Ocaml")]) in
	let b71 = Balise (B, [], [b7]) in
	let b72 = Balise (I, [], [Text ("un langage")]) in
	let b73 = Balise (Empty, [], [Text ("multi-paradigmes")]) in
	let b74 = Balise (H 1, [Alg "center"], [b71; b72; b73]) in 
	let b6 = Balise (B, [], [Text ("fonctionnel")]) in
	let b61 = Balise (Li, [], [b6]) in 
	let b62 = Balise (Li, [], [Text ("imperatif")]) in
	let b63 = Balise (Li, [], [Text ("a objets")]) in
	let b5 = Balise (Ul, [], [b61; b62; b63]) in
	let b4 = Balise (H 2, [], [Text ("Apprendre en programmant c'est plus amusant")]) in
	let b3 = Balise (Body, [], [b74; b5; b4]) in
	let b2 = Balise (Title, [], [Text ("Le langage Ocaml")]) in
	let b21 = Balise (Link, [Href "mystyle.css"], []) in
	let b22 = Balise (Head, [], [b2; b21]) in*)