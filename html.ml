type attribut = Alg of string

type attributs = attribut list

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

type arbre =
	| Text of string
	| Balise of balise * attributs * arbre list

type document_html = {nom: string; document: arbre}

let rec tree_to_string tree =
	match tree with
	  Text s -> s
	| Balise (b, a, t) -> (match b with
												   Html -> "<html " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</html>"
												 | Title -> "<title " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</title>\n"
												 | Head -> "<head " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</head>\n"
												 | Body -> "<body " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</body>\n"
												 | H i -> "<h" ^ string_of_int i ^ " " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</h" ^ string_of_int i ^ ">\n"
												 | B -> "<b " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</b>\n"
												 | I -> "<i " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</i>\n"
												 | U -> "<u " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</u>\n"
												 | Del -> "<del " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</del>\n"
												 | Ul -> "<ul " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</ul>\n"
												 | Li -> "<li " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</li>\n"
												 | P -> "<p " ^ attributes_to_string a ^ ">\n" ^ tree_to_list t ^ "</p>\n"
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

let init_arbre =
	let fic = open_out "res.html" in
	let b7 = Balise (Li, [], [Text ("je suis le body2\n")]) in 
	let b6 = Balise (Li, [], [Text ("je suis le body1\n")]) in 
	let b5 = Balise (Ul, [], [b6; b7]) in
	let b4 = Balise (Body, [], [b5]) in
	let b3 = Balise (Title, [], [Text ("titre de la page\n")]) in
	let b2 = Balise (Head, [], [b3; Text ("je suis le head<br/>\n")]) in
	let b1 = Balise (Html, [], [b2; b4]) in
	output_string fic (tree_to_string b1);
	close_out fic