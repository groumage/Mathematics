open Function
open Graph

let eqn_of_string s  = Parser.yacc_eqn  Lexer.lexer_main (Lexing.from_string s)
let expr_of_string s = Parser.yacc_expr Lexer.lexer_main (Lexing.from_string s)

let parse_eqn  = eqn_of_string
let parse_expr = expr_of_string

let _ = Html.init_arbre
	(*let loop choice =
		match choice with
  			| 1 ->	let _ = Sys.command "clear" in
  					let matrix = Matrix.get_matrix "MATRIX CALCULATOR"
					in
  					let _ = Sys.command "clear" in
  					print_string "MATRIX CALCULATOR\n\n";
  					let nb_lines = string_of_int (Matrix.nb_lines matrix)
  					in
  					let nb_columns = string_of_int (Matrix.nb_columns matrix)
  					in
  					Graph.create_matrix matrix Graph.length_x_matrix Graph.length_y_matrix;
  					print_string ("Dimensions:\n" ^ nb_lines ^ "x" ^ nb_columns);
					print_string "\n\n";
					print_string "Matrix:\n";
					Matrix.print_matrix matrix;
					print_string "\n";
					print_string "Transpose:\n";
					Matrix.print_matrix (Matrix.transpose matrix);
					print_string "\n";
					print_string ("Determinant:\n" ^ string_of_float (Matrix.determinant matrix));
					print_string "\n\n";
					print_string ("Trace:\n" ^ string_of_float (Matrix.trace matrix));
					print_string "\n";
					choice
			| 2 -> 	let _ = Sys.command "clear" in
					let matrix1 = Matrix.get_matrix "MATRIX MULTIPLICATOR"
					in
					let _ = Sys.command "clear"
					in 
					let matrix2 = Matrix.get_matrix "MATRIX MULTIICATOR"
					in
					let _ = Sys.command "clear"
					in
					print_string "MATRIX MULTIPLICATOR\n\n";
					let nb_lines_1 = Matrix.nb_lines matrix1
					in
					let nb_columns_1 = Matrix.nb_columns matrix1
					in
					print_string ("Dimensions:\n" ^ string_of_int nb_lines_1 ^ "x" ^ string_of_int nb_columns_1);
					print_string "\n\n";
					print_string "Matrix:\n";
					Matrix.print_matrix matrix1;
					print_string "\n";
					let nb_lines_2 = Matrix.nb_lines matrix2
					in
					let nb_columns_2 = Matrix.nb_columns matrix2
					in
					print_string ("Dimensions:\n" ^ string_of_int nb_lines_2 ^ "x" ^ string_of_int nb_columns_2);
					print_string "\n\n";
					print_string "Matrix:\n";
					Matrix.print_matrix matrix2;
					print_string "\n";
					print_string "ADD:\n";
					Matrix.print_matrix (Matrix.add_matrix matrix1 matrix2);
					print_string "\n";
					print_string "MULTIPLY:\n";
					Matrix.print_matrix (Matrix.mult_matrix matrix1 matrix2);
					print_string "\n";
					choice
			| 3 -> 	let _ = Sys.command "clear"
					in
					print_string "GRAPHICS\n\n";
					print_string "Enter the function:\n";
					print_endline "f(x) = ";
					(*let lexbuf = Lexing.from_channel stdin
					in
					let result = Parser.main Lexer.token lexbuf
      				in
      				let fct_formel = ref result
      				in
      				fct_formel := simplify_expr (parse_expr (!fct_formel));
						begin
							let fen = Graph.create_graph (Graph.length_x + 2) (Graph.length_y + 2) !Graph.min_abs !Graph.max_abs !Graph.min_ord !Graph.max_ord
							in
							let tab_x = Graph.decoupe_intervalle fen.abs !Graph.nombre_points
							in
		      				let tab_y = Graph.decoupe_intervalle fen.ord !Graph.nombre_points
		      				in
			      			let map_tab_y = ref (Graph.map !fct_formel tab_y)
			      			in
			      			let zip_tab = ref (Graph.zip tab_x !map_tab_y)
			      			in
			      			Graph.draw_center_lines_graph (Graph.length_x + 2) (Graph.length_y + 2) !Graph.min_abs !Graph.max_abs !Graph.min_ord !Graph.max_ord;
							Graph.trace (Graph.en_tableau_pixel fen !zip_tab) Graphics.blue;
							Graph.draw_frame_graph (Graph.length_x + 2) (Graph.length_y + 2);
							Graph.print_fct !fct_formel;
							let e = ref (Graphics.wait_next_event [Graphics.Key_pressed])
							in
							let loop = ref true
							in
							while !loop do
								match !e.Graphics.key with
									| 'q' ->	begin
													loop := false;
													Graphics.clear_graph ()
												end	
									| 'd' ->	begin
													Graphics.clear_graph();
													fct_formel := Function.deriv !fct_formel "x";
													(*fct_formel := Function.simplify !fct_formel;*)
													map_tab_y := Graph.map !fct_formel tab_y;
													zip_tab := Graph.zip tab_x !map_tab_y;
													Graph.draw_center_lines_graph (Graph.length_x + 2) (Graph.length_y + 2) !Graph.min_abs !Graph.max_abs !Graph.min_ord !Graph.max_ord;
													Graph.trace (Graph.en_tableau_pixel fen !zip_tab) Graphics.red;
													Graph.draw_frame_graph (Graph.length_x + 2) (Graph.length_y + 2);
													Graph.print_fct !fct_formel;
													e := Graphics.wait_next_event [Graphics.Key_pressed]
												end
									| 'i' ->	begin
													Graphics.clear_graph();
													fct_formel := Function.integrate !fct_formel "x";
													map_tab_y := Graph.map !fct_formel tab_y;
													zip_tab := Graph.zip tab_x !map_tab_y;
													Graph.draw_center_lines_graph (Graph.length_x + 2) (Graph.length_y + 2) !Graph.min_abs !Graph.max_abs !Graph.min_ord !Graph.max_ord;
													Graph.trace (Graph.en_tableau_pixel fen !zip_tab) Graphics.green;
													Graph.draw_frame_graph (Graph.length_x + 2) (Graph.length_y + 2);
													e := Graphics.wait_next_event [Graphics.Key_pressed]
												end
									| _ ->	e := Graphics.wait_next_event [Graphics.Key_pressed]
							done;*)
							choice
						(*end*)
			| 5 -> 	print_string "\nSee you next time !\n";
					choice
  			| _ -> 	failwith "loop: wrong choice" 
  	in
	let res = ref 0
	in
	while !res != 5 do
		if !res == 0
		then print_string "1. Matrix calculator\n"
		else print_string "\n1. Matrix calculator\n";
		print_string "2. Matrix multiplicator\n";
		print_string "3. Graphic\n";
		print_string "4. Function calcualtor\n";
		print_string "5. Exit\n";
		(*print_int (List.length (Function.factorise (Plus (Var "x", Minus (Times (2, Var "x"), Var "x")))));*)
		res := loop (read_int ())
	done*)