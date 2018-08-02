type attributs = (string * string) list

type balise =
	| Html
	| Title
	| Head
	| Body
	| Link
	| H of int
	| B
	| I
	| U
	| Ul
	| Li

type arbre =
	| Text of string
	| Balise of balise * attributs * arbre list

type document_html = {nom: string; document: arbre}

let rec node_to_html n =
	match n with
		| [] -> ""
		| h :: t -> (
						match h with
							| [] -> match b with
										| Htlm -> "<html>\n</html>" 
										| Title -> "<title></title>\n" 
										| Head -> "<head>\n</head>" 
										| Body -> "<hbody>\n</body>" 
							| h :: t -> 
					)

and arbre_to_html tree =
	match tree with
		| Text t -> t
		| Balise (b, a, t) -> node_to_html t		

let init_arbre title =
	let fic = open_out "res.html" in
	let b4 = Balise (Body, [], [Text ("je suis le body")]) in
	let b3 = Balise (Title, [], [Text (title), Text ("je suis le head")]) in
	let b2 = Balise (Head, [], [b3]) in
	let b1 = Balise (Html, [], []) in
	output_string fic ("toto");
	close_out fic