type attribut = Alg of string
							| Href of string
							| Type of string
							| Src of string
							| Id of string
							| Rows of int
							| Cols of int
							
type attributs = attribut list

type style = Color of string
					 | FontSize of int
					 | BackgroundColor of string
	
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
let m = M.add (H 2) ([Color "navy"; BackgroundColor "yellow"]) m
let m = M.add (Li) ([FontSize 10]) m

let style_to_string2 s =
	match s with
	  Color s -> "color : " ^ s ^ ";"
	| FontSize i -> "font-size : " ^ string_of_int i ^ "px;"
	| BackgroundColor s -> "background-color : " ^ s ^ ";"

let rec tree_to_string tree =
	match tree with
	  Text s -> s
	| Balise (b, a, t) -> (match b with
												   Html -> "<html " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</html>"
												 | Title -> "<title " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</title>\n"
												 | Head -> "<head " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</head>\n"
												 | Body -> "<body " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</body>\n"
												 | Link -> "<link " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</link>\n"
												 | H i -> "<h" ^ string_of_int i ^ " " ^ attributes_to_string a ^ "style = \"" ^ style_to_string (H i) ^ "\">" ^ tree_to_list t ^ "</h" ^ string_of_int i ^ ">\n"
												 | B -> "<b " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</b>\n"
												 | I -> "<i " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</i>\n"
												 | U -> "<u " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</u>\n"
												 | Del -> "<del " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</del>\n"
												 | Ul -> "<ul " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</ul>\n"
												 | P -> "<p " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</p>\n"
												 | Li -> "<li " ^ attributes_to_string a ^ "style = \"" ^ style_to_string Li ^ "\">" ^ tree_to_list t ^ "</li>\n"
												 | Script -> "<script " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</script>\n"
												 | Div -> "<div " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</div>\n"
												 | Textarea -> "<textarea " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</textarea>\n"
												 | Button -> "<button " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</button>\n"
												 | Empty -> attributes_to_string a ^ tree_to_list t
												)
and tree_to_list tree_list =
	match tree_list with
		[] -> ""
	| h :: t -> tree_to_string h ^ tree_to_list t
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
and style_to_string balise =
	let a = M.find balise m in
	let rec decompose_lst lst =
		match lst with
			[] -> ""
		| h :: t -> style_to_string2 h ^ " " ^ decompose_lst t
	in
	decompose_lst a

let init_arbre =
	let fic = open_out "res.html" in
	let title = Balise (Title, [], [Text "Test js_of_ocaml\n"]) in
	let script = Balise (Script, [Type "text/javascript"; Src "test.js"], []) in
	let head = Balise (Head, [], [title; script]) in
	let h1 = Balise (H 1, [], [Text "Mathemataume's project"]) in
	let p1 = Balise (P, [], [Text "Matrix"]) in
	let text1 = Balise (Textarea, [Id "input_1"; Rows 2; Cols 40], []) in
	let btn1 = Balise (Button, [Id "btn_1"], []) in
	let p2 = Balise (P, [], [Text "Simplify expressions"]) in
	let text2 = Balise (Textarea, [Id "input_2"; Rows 2; Cols 40], []) in
	let btn2 = Balise (Button, [Id "btn_2"], []) in
	let text3 = Balise (Textarea, [Id "output_1"; Rows 2; Cols 40], []) in
	let body = Balise (Body, [], [h1; p1; text1; btn1; p2; text2; btn2; text3]) in
	let html = Balise (Html, [], [head; body]) in
	output_string fic (tree_to_string html);
	close_out fic
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