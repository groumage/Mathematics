open Function
open Graph
	
let main =
	let loop choice =
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
			| 3 -> 	let _ = Sys.command "clear" in
					print_string "Enter the function:\n";
					print_endline "f(x) = ";
					let lexbuf = Lexing.from_channel stdin
					in
					let result = Parser.main Lexer.token lexbuf
      				in
      				let fct_formel = result
      				in
      				if (Function.free_variables_present fct_formel "x")
					then
						begin 
							print_string "Free variables are presents.\n";
							choice
						end
					else
						begin
							let fen = Graph.create_fenetre 901 701 (-10.) 10. (-10.) 10.
							in
							let tab_x = Graph.decoupe_intervalle fen.abs !Graph.nombre_points
							in
		      				let tab_y = Graph.decoupe_intervalle fen.ord !Graph.nombre_points
		      				in
			      			let map_tab_y = Graph.map fct_formel tab_y
			      			in
			      			let zip_tab = Graph.zip tab_x map_tab_y
			      			in
							Graph.trace (Graph.en_tableau_pixel fen zip_tab) Graphics.blue;
							choice
						end
						(*let e = Graphics.wait_next_event [Graphics.Button_down] in
							Graphics.clear_graph();
							let fct_formel = Function.deriv (Add (Cos (Sin (Cos (Sin (Var("x"))))), Float 5.)) "x" in
							let map_tab_y = Function.map (fct_formel) tab_y in
							let zip_tab = Function.zip tab_x map_tab_y in
							Function.trace (Function.en_tableau_pixel fen zip_tab) Graphics.blue;*)
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
		print_string "5. Exit\n"; res := loop (read_int ())
	done