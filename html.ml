type attribut = Alg of string
							| Href of string
							

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
let m = M.add (H 1) (Color "blue") m
let m = M.add (H 2) ([Color "navy"; BackgroundColor "yellow"])
let m = M.add (Li) (FontSize 10)

let rec tree_to_string tree =
	match tree with
	  Text s -> s
	| Balise (b, a, t) -> (match b with
												   Html -> "<html " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</html>"
												 | Title -> "<title " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</title>\n"
												 | Head -> "<head " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</head>\n"
												 | Body -> "<body " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</body>\n"
												 | Link -> "<link " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</link>\n"
												 | H i -> "<h" ^ string_of_int i ^ " " ^ attributes_to_string a ^ " style= \"" ^ style_to_string (H i) "\">" ^ tree_to_list t ^ "</h" ^ string_of_int i ^ ">\n"
												 | B -> "<b " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</b>\n"
												 | I -> "<i " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</i>\n"
												 | U -> "<u " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</u>\n"
												 | Del -> "<del " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</del>\n"
												 | Ul -> "<ul " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</ul>\n"
												 | Li -> "<li " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</li>\n"
												 | P -> "<p " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</p>\n"
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
								Alg s -> "align=\"" ^ s ^ "\" " ^ attributes_to_string t
							| Href s -> "href=" ^ s ^ " " ^ attributes_to_string t
and style_to_string

let init_arbre =
	let fic = open_out "res.html" in
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
	let b22 = Balise (Head, [], [b2; b21]) in
	let b1 = Balise (Html, [], [b22; b3]) in
	output_string fic (tree_to_string b1);
	close_out fic