(* Create a matrix with n lines and p columns *)
let make_matrix n p =
	let m = Array.make n [||] in
	for i = 0 to (n-1) do
		m.(i) <- Array.make p 0.
	done;
m

(* Return the number of lines of the matrix m *)
let nb_lines m =
	Array.length m

(* Return the number of columns of the matrix m *)
let nb_columns m =
	Array.length m.(0)

(* Fill the line i of the matrix m with the function f *)
let fill_line m i f =
	let p = nb_columns m in
	for j = 0 to (p-1) do
		m.(i).(j) <- f j
	done

(* Fill all the matrix m with the function f *)
let fill_matrix m f =
	let n = nb_lines m in
	for i = 0 to (n-1) do
		fill_line m i (function j -> f (i, j))
	done

(* Return the matrix identity of dimension n *)
let identity n =
	let m = make_matrix n n in
	let n = nb_lines m in
	for i = 0 to (n-1) do
		fill_matrix m (function i, j -> if i = j then 1. else 0.)
	done;
m

(* Return a new matrix which is the copy of matrix m *)
let copy_matrix m =
	let m2 = make_matrix (nb_lines m) (nb_columns m) in
	fill_matrix m2 (function i, j -> m.(i).(j));
m2

(* Return matrix-line which has n columns *)
let integers n =
	let m = make_matrix 1 n in
	fill_matrix m (function i, j -> float_of_int(j+1));
m

let str_matrix_line m i =
	let str = ref "" in
	let p = nb_columns m in
	for j = 0 to (p-1) do
		str := !str ^ " " ^ string_of_float m.(i).(j)
	done;
!str

(* Return a string format of the matrix m *)
let str_of_matrix m =
	let str = ref "" in
	let n = nb_lines m in
	let p = nb_columns m in
	for i = 0 to (n-1) do
		for j = 0 to (p-1) do
			if Util.is_float_int m.(i).(j) then
				str := !str ^ " " ^ string_of_int (int_of_float m.(i).(j))
			else
				str := !str ^ " " ^ string_of_float m.(i).(j)
		done;
		str := !str ^ "\n"
	done;
!str

(* Print the matrix m on the console *)
let print_matrix m =
	print_string (str_of_matrix m)

(* Return a matrix which contains all elements of the list l *)
let matrix_of_vector v =
	let m = make_matrix 1 (List.length v) in
	let rec fill i v = match v with
		| [] -> ()
		| h :: t -> begin m.(0).(i) <- h; fill (i+1) t end
	in
fill 0 v;
m

let matrix_of_list l =
	let m = make_matrix (List.length l) (List.length (List.hd l)) in
	let rec fill_list i j l =
		match l with
  		| [] -> ()
  		| h :: t ->
				begin
					let rec fill_sub_list i j l2 =
						match l2 with	
							|  [] -> fill_list (i+1) 0 t
							| h2 :: t2 -> m.(i).(j) <- h2; fill_sub_list i (j+1) t2
					in
					fill_sub_list i 0 h
				end
	in
fill_list 0 0 l;
m

(* Return a list which contains all elements of the vector v (vector = matrix-line) *)
let list_of_vector v =
	let rec list_of_vector_acc acc = function
		| -1 -> acc
		| j -> list_of_vector_acc (v.(j) :: acc) (j-1)
	in
list_of_vector_acc [] ((Array.length v) - 1)

(* Return a list of list which contains all elements of the matrix m *)
let list_of_matrix m =
	let rec list_of_matrix_acc acc m = function
		| -1 -> acc
		| i -> list_of_matrix_acc ((list_of_vector m.(i)) :: acc) m (i-1)
	in
list_of_matrix_acc [] m (nb_lines m -1)

(* Return a new matrix which is the transpose of the matrix m *)
let transpose m =
	let m2 = make_matrix (nb_lines m) (nb_columns m) in
	fill_matrix m2 (function i, j -> m.(j).(i));
m2

(* Transform the matrix m :*)
(* 1 2 3    -->    12 11 10*)
(* 4 5 6           9 8 7   *)
(* 7 8 9           6 5 4   *)
(* 10 11 12        3 2 1   *)
let double_rotate m =
	let n = nb_lines m in
	let p = nb_columns m in
	let m2 = make_matrix n p in
	fill_matrix m2 (function i, j -> m.(n-i-1).(p-j-1));
m2

(* Return a new matrix which is the sum oh matrixs m1 and m2 *)
let add_matrix m1 m2 =
	let n1 = nb_lines m1 in
	let p1 = nb_columns m1 in
	let n2 = nb_lines m2 in
	let p2 = nb_columns m2 in
	if (n1 != n2 || p1 != p2)
	then failwith "add_matrix: wrong dimensions"
	else let m3 = make_matrix n1 p1 in
	fill_matrix m3 (function i, j -> m1.(i).(j) +. m2.(i).(j));
m3

(* Return the trace of the matrix *)
let trace m =
	let n = nb_lines m in
	let p = nb_columns m in
	if n != p
	then failwith "trace: wrong dimensions"
	else let somme = ref 0. in
	for i = 0 to (n-1) do
		somme := !somme +. m.(i).(i);
	done;
!somme

let mult_matrix_const m k =
	let m2 = make_matrix (nb_lines m) (nb_columns m) in
	fill_matrix m2 (function i, j -> k *. m.(i).(j));
m2

let mult_matrix m1 m2 =
	let n1 = nb_lines m1 in
	let p1 = nb_columns m1 in
	let n2 = nb_lines m2 in
	let p2 = nb_columns m2 in
	if p1 != n2
	then failwith "mult_matrix: wrong dimensions"
	else let m3 = make_matrix n1 p2 in
	let fill_function (i,j) =
  	begin
  		let total = ref 0. in
  		for k = 0 to (p1-1) do
  			total := !total +. m1.(i).(k) *. m2.(k).(j);
  		done;
  		!total;
  	end
	in
	fill_matrix m3 fill_function;
m3

let rec power_matrix m p =
	if p = 1
 	then identity (nb_columns m)
	else mult_matrix m (power_matrix m (p-1))

let swap_items m (l1,c1) (l2,c2) =
	let mem = m.(l1).(c1) in
	m.(l1).(c1) <- m.(l2).(c2);
	m.(l2).(c2) <- mem

let swap_lines m i k =
	let p = nb_columns m in
	for j = 0 to p-1 do
		swap_items m (i,j) (k,j);
	done

let reduce_diagonal m inv =
	let n = nb_lines m in
	for i = 0 to n-1 do
		let coeff = m.(i).(i) in
		fill_line m i (function j -> m.(i).(j) /. coeff);
		fill_line inv i (function j -> inv.(i).(j) /. coeff);
	done

let add_linear_combination m (i1, c1) (i2,c2) =
	fill_line m i1 (function j -> c1 *. m.(i1).(j) +. c2 *. m.(i2).(j))

let pivot m inv (a,b) =
	let n = nb_lines m in
	let valeur_pivot = m.(a).(b) in
	for i = a+1 to n-1 do
		let r = -.m.(i).(b) in
		add_linear_combination inv (i,valeur_pivot) (a,r);
		add_linear_combination m (i,valeur_pivot) (a,r);
	done
	(* Li <- valeur_pivot*Li + r*La 
	   valeur_pivot = le premier coeff de la ligne a (La)
	   -r = -premier coeff de la ligne i (Li)
	   donc valeur_pivot*Li + r*La = 0
	   donc il y a des 0 en dessous du pivot
	   *)

let find_pivot m i =
	let k = ref i in
	let n = nb_lines m in
	while (!k < n  && m.(!k).(i) = 0.) do
		k := !k + 1;
	done;
	if !k = n
	then
	failwith "find_pivot: singular matrix"
	else
!k

let rec apply_function f x = function
	| 0 -> x
	| n -> apply_function f (f x) (n-1)

let iteration_pivot m inv i =
	let k = find_pivot m i in
	if i != k
	then swap_lines m i k;
	swap_lines inv i k;
	pivot m inv (i,i)

let invert_matrix m =
	let n = nb_lines m in
	let reduce_and_rotate (a,b) =
	begin
		for k = 0 to n-2 do
			iteration_pivot a b k;
		done;
		(double_rotate a, double_rotate b)
	end
	in
	let (m,inv) = apply_function reduce_and_rotate (copy_matrix m, identity n) 2 in
	reduce_diagonal m inv;
inv

let solve_system m y =
	mult_matrix (invert_matrix m) y
	
let reduce_matrix i j m =
	let n = nb_lines m in
	let p = nb_columns m in
	let m_reduce = make_matrix (n-1) (p-1) in
	let cpt_lines = ref 1 in
	let cpt_columns = ref 1 in
	for k = 1 to n do
		for l = 1 to p do
			if k != i && j != l
			then
				begin
					m_reduce.(!cpt_lines-1).(!cpt_columns-1) <- m.(k-1).(l-1);
  				cpt_columns := !cpt_columns + 1;
					if !cpt_columns == n
  				then
  					cpt_lines := !cpt_lines + 1;
				end
		done;
		cpt_columns := 1;
	done;
m_reduce
	
let rec determinant m =
	let line_selected = 1 in
	let n = nb_lines m in
	let p = nb_columns m in
	if n != p
	then
		failwith "determinant:wrong dimensions"
	else
		if n = 2
		then
			m.(0).(0) *. m.(1).(1) -. m.(1).(0) *. m.(0).(1)
		else
			let somme = ref 0. in
			for indice_col = 1 to p do
				somme := !somme +. (Util.power (-1.) (1. +. float_of_int(indice_col))) *. m.(line_selected-1).(indice_col-1) *. determinant (reduce_matrix line_selected indice_col m);
			done;
		!somme
			
let split expr s =
	let length = ref (String.length s) in
	if !length != 0 then
		let mem1 = ref 0 in
		let mem2 = ref 0 in
		let l = ref [] in
		let str = ref "" in
		if s.[!length - 1] != '\n' then (str := !str ^ (s ^ String.make 1 '\n'); length := !length + 1) else str := !str ^ s;
		while !mem1 != !length - 1 do
			while !str.[!mem2] != expr && !mem2 != !length - 1 do
				mem2 := !mem2 + 1;
			done;
			l := !l @ [String.sub !str !mem1 (!mem2 - !mem1)];
			while !str.[!mem2] = expr && !mem2 != !length - 1 do
				mem2 := !mem2 + 1;
			done;
			mem1 := !mem2;
		done;
		!l
	else
	[]

let get_matrix string_matrix =
	let matrix_to_lines = split '\n' string_matrix in
	let nb_lines = List.length matrix_to_lines in
	if nb_lines > 0 then
		let nb_columns = List.length (split ' ' (List.nth matrix_to_lines 0)) in
		let matrix = make_matrix nb_lines nb_columns in
		for i = 1 to nb_lines do
			let line_to_split = List.nth matrix_to_lines (i - 1)
			in
			let line_to_matrix = split ' ' line_to_split
			in
			let rec split_line j line =
				match line with
					| [] -> ()
					| h :: t ->	matrix.(i - 1).(j - 1) <- float_of_string h;
								split_line (j + 1) t
			in
			split_line 1 line_to_matrix
		done;
	matrix
else
	failwith ("This is not a matrix:" ^ string_matrix)

let is_matrix string_matrix =
	let matrix_to_lines = split '\n' string_matrix in
	let nb_lines = List.length matrix_to_lines in
	if nb_lines == 0 then false
	else
		let nb_columns = List.length (split ' ' (List.nth matrix_to_lines 0)) in
		let res = ref true in
		for i = 0 to nb_lines - 1 do
			res := !res && (List.length (split ' ' (List.nth matrix_to_lines i)) == nb_columns)
		done;
	!res