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

let fic = open_out "res.html";;
output_string fic ("<html>" ^ string_of_float (5.5) ^ "</html>");;
close_out fic;;
let _ = Sys.command "firefox res.html";;