type formel =
	| Float of float
	| Var of string
	| Add of formel * formel
	| Sub of formel * formel
	| Mul of formel * formel
	| Div of formel * formel
	| Ln of formel
	| Cos of formel
	| Sin of formel
	| Puis of formel * formel
	| Sqrt of formel
	| Exp of formel

type subtree =
	| Plus of formel
	| Minus of formel
	| Times of int * formel

let rec string_fct f =
	match f with
  		| Float f -> string_of_float f
  		| Var v -> v
		| Mul (f, g) -> "(" ^ string_fct f ^ " * " ^ string_fct g ^ ")"
		| Div (f, g) -> "(" ^ string_fct f ^ " / " ^ string_fct g ^ ")"
		| Add (f, g) -> "(" ^ string_fct f ^ " + " ^ string_fct g ^ ")"
		| Sub (f, g) -> string_fct f ^ " - " ^ string_fct g
		| Ln f -> "ln (" ^ string_fct f ^ ")"
		| Cos f -> "cos (" ^ string_fct f ^ ")"
		| Sin f -> "sin (" ^ string_fct f ^ ")"
		| Puis (f, g) -> "(" ^ string_fct f ^ " ^ (" ^ string_fct g ^ "))"
		| Sqrt f -> "sqrt (" ^ string_fct f ^ ")"
		| Exp f -> "exp (" ^ string_fct f ^ ")"

(*let rec simplify f =
	let f_simplify = simp f in
	if f_simplify = f 
	then f_simplify 
	else simplify f_simplify
	and simp f = 
		match f with
			| Float f -> Float f
			| Var x -> Var x
			
			(* 1 * x -> x *)
			| Mul (Float 1., f) -> simp f
			(* x * 1 -> x *)
			| Mul (f, Float 1.) -> simp f
			(* 0 * x -> 0 *)
			| Mul (Float 0., f) -> Float 0.
			(* x * 0 -> 0 *)
			| Mul (f, Float 0.) -> Float 0.
			(* f1 * f2 -> calcul (f1 * f2) *)
			| Mul (Float f1, Float f2) -> Float (f1 *. f2)
			
			(* 0 / x -> 0 *)
			| Div (Float 0., f) -> Float 0.
			(* x / 1 -> x *)
			| Div (f, Float 1.) -> simp f
			(* f1 / f2 -> calcul (f1 / f2) *)
			| Div (Float f1, Float f2) -> Float (f1 /. f2)
			(* x / x -> 1 *)
			| Div (f, g) when f = g -> Float 1.
			
			| Div (f, g) -> Div (simp f, simp g)
			
			(* x * x -> x ^ 2 *)
			| Mul (f, g) when f = g -> simp (Puis (simp f, Float 2.))
			(* x ^ a * x -> x ^ (a + 1) *)
			| Mul (Puis (f, g), h) when f = h -> Puis (simp f, simp (Add (simp g, Float 1.)))
			
			(* (f1 * x) * f2 -> (f1 * f2) * x *)
			| Mul (Mul (Float f1, f), Float f2) -> simp (Mul (Float (f1 *. f2), simp f))
			(* f1 * (f2 * x) -> (f1 * f2) * x *)
			| Mul (Float f1, Mul (Float f2, f)) -> simp (Mul (Float (f1 *. f2), simp f))
			(* (x * f1) * f2 -> (f1 * f2) * x *)
			| Mul (Mul (f, Float f1), Float f2) -> simp (Mul (Float (f1 *. f2), simp f))
			(* f1 * (x * f2) -> (f1 * f2) * x *)
			| Mul (Float f1, Mul (f, Float f2)) -> simp (Mul (Float (f1 *. f2), simp f))

			| Mul (f, g) -> Mul (simp f, simp g)
			(* 0 + x -> x *)
			| Add (Float 0., f) -> simp f
			(* x + 0 -> x *)
			| Add (f, Float 0.) -> simp f
			(* f1 + f2-> calcul (f1 + f2) *)
			| Add (Float f1, Float f2) -> Float (f1 +. f2)
			(* x + x -> 2 * x *)
			| Add (f, g) when f = g -> simp (Mul (Float 2., simp f))
			
			(* f1 * x + x -> (f1 + 1) * x *)
			| Add (Mul (Float f1, f), g) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))
			(* x + f1 * x -> (f1 + 1) * x *)
			| Add (f, Mul (Float f1, g)) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))
			(* x * f1 + x -> (f1 + 1) * x *)
			| Add (Mul (f, Float f1), g) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))
			(* x + x * f1 -> (f1 + 1) * x *)
			| Add (f, Mul (g, Float f1)) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))

			(* f1 * x + f2 * x -> (f1 + f2) * x *)
			| Add (Mul (Float f1, f), Mul (Float f2, g)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))
			(* x * f1 + f2 * x -> (f1 + f2) * x *)
			| Add (Mul (f, Float f1), Mul (Float f2, g)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))
			(* f1 * x + x * f2 -> (f1 + f2) * x *)
			| Add (Mul (Float f1, f), Mul (g, Float f2)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))
			(* x * f1 + x * f2 -> (f1 + f2) * x *)
			| Add (Mul (f, Float f1), Mul (g, Float f2)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))

			| Add (f, g) -> Add (simp f, simp g)

			(* 0 - x -> - x *)
			| Sub (Float 0., f) -> simp (Mul (Float (-1.), simp f))
			(* x - 0 -> x *)
			| Sub (f, Float 0.) -> simp f
			(* f1 - f2 -> calcul (f1 - f2) *)
			| Sub (Float f1, Float f2) -> Float (f1 -. f2)
			(* f1 * x + x -> (f1 + 1) * x *)
			| Sub (f, g) when f = g -> Float 0.
			
			| Sub (f, g) -> Sub (simp f, simp g)

			
			(* x ^ 0 -> 1 *)
			| Puis (f, Float 0.) -> Float 1.
			(* 0 ^ x -> 0 *)
			| Puis (Float 0., f) -> Float 0.
			(* x ^ 1 -> x *)
			| Puis (f, Float 1.) -> simp f

			| Puis (f, g) -> Puis (simp f, simp g)
			
			| Ln f -> Ln (simp f)
			| Cos f -> Cos (simp f)
			| Sin f -> Sin (simp f)
			| Sqrt f -> Sqrt (simp f)
			| Exp f -> Exp (simp f)*)

	(*List.filter ( fun f -> match f with Plus f -> (is_in (Minus(f)) l) == false | Minus f -> (is_in (Plus(f)) l ) == false) l*)
let rec tree_to_list f i =
	match (f, i) with
		| (Add (f, g), _) -> tree_to_list f 0 @ tree_to_list g 0
		| (Sub (f, g), _) -> tree_to_list f 0 @ tree_to_list g 1
		| (f, 0) -> [Plus f]
		| (f, 1) -> [Minus f]
		| _ -> failwith "impossible 1"

let rec is_in f l =
	match l with
    	| [] -> false
    	| h :: t -> if h = f then true else is_in f t

let rec remove f l =
	match l with
		| [] -> []
		| h :: t when h = f -> t
		| h :: t -> h :: remove f t

let rec remove_all f l =
	match l with
		| [] -> []
		| h :: t when h = f -> remove_all f t
		| h :: t -> h :: remove_all f t

let rec count f l =
	match l with
		| [] -> 0
		| h :: t when h = f -> 1 + count f t
		| h :: t -> count f t

let rec factorise l =
	match l with
		| [] -> []
		| (Plus f) :: t -> let cpt = (1 + count (Plus(f)) t) in if cpt > 1 then [Times (cpt, f)] @ factorise (remove_all (Plus(f)) t) else [Plus f] @ factorise t
		| (Minus f) :: t -> let cpt = (1 + count (Minus(f)) t) in if cpt > 1 then [Times (-cpt, f)] @ factorise (remove_all (Minus(f)) t) else [Minus f] @ factorise t

let rec filter l =
	match l with
		| [] -> []
		| (Plus f) :: t -> if is_in (Minus(f)) t then filter (remove (Minus(f)) t) else [Plus f] @ filter t
		| (Minus f) :: t -> if is_in (Plus(f)) t then filter (remove (Plus (f)) t) else [Minus f] @ filter t

let rec list_to_tree l =
	match l with
		| [] -> Float 0.
		| [Plus f] -> f
		| [Minus f] -> Mul (Float (-1.), f)
		| [Times (i, f)] -> Mul (Float (float_of_int i), f)
		| (Plus f) :: t -> Add (f, list_to_tree t)
		| (Minus f) :: t -> Sub (list_to_tree t, f)
		| (Times (i, f)) :: t -> Add (Mul (Float (float_of_int i), f), list_to_tree t)

let rec simplify f =
	let f_simplify = simp f in
	if f_simplify = f
	then f_simplify 
	else simplify f_simplify
	and simp f = 
		match f with
			| Float f -> Float f
			| Var x -> Var x

			(* 0 + x -> x *)
			| Add (Float 0., f) -> simp f
			(* x + 0 -> x *)
			| Add (f, Float 0.) -> simp f
			(* f1 + f2-> calcul (f1 + f2) *)
			| Add (Float f1, Float f2) -> Float (f1 +. f2)

			| Add (f, g) -> list_to_tree (factorise (filter (tree_to_list (Add (f, g)) 0)))
			| Sub (f, g) -> list_to_tree (factorise (filter (tree_to_list (Sub (f, g)) 0)))
			| Mul (f, g) -> Mul (simp f, simp g)

			(*
			| Add (f, Mul (Float f1, g)) when f = g -> simp (Mul (Float (f1 +. 1.), f))
			| Add (f, Mul (g, Float f1)) when f = g -> simp (Mul (Float (f1 +. 1.), f))
			| Add (Mul (Float f1, f), g) when f = g -> simp (Mul (Float (f1 +. 1.), f))
			| Add (Mul (f, Float f1), g) when f = g -> simp (Mul (Float (f1 +. 1.), f))*)

(*let rec sub_tree_list f i =
	match (f,i) with
		| (f, 0) -> [Plus f]
		| (f, 1) -> [Minus f]
		| (Add (f, g), _) -> (sub_tree_list f 0) @ (sub_tree_list f 0)
		| (Sub (f, g), _) -> (sub_tree_list f 1) @ (sub_tree_list f 1)
		| _ -> failwith "error"

let rec is_in f l =
	match l with
		| [] -> false
		| h :: _ when h = f -> true
		| h :: t -> is_in f t

let rec reduce_list_subtree l =
	match l with
		| [] -> []
		| h :: t -> (
					match h with
						| Plus f -> if is_in (Minus f) l 
									then reduce_list_subtree (List.filter (is_in (Plus f) == false && is_in (Minus f) == false) l) 
									else reduce_list_subtree t
						| Minus f -> if is_in (Plus f) l 
									then reduce_list_subtree (List.filter (is_in (Plus f) == false && is_in (Minus f) == false) l) 
									else reduce_list_subtree t
					)
	*)

let rec deriv f x =
	match f with
  		| Float f -> Float 0.
  		| Var v when v = x -> Float 1.
  		| Var v -> Float 0.
		| Add (f, g) ->	Add (deriv f x, deriv g x)
		| Sub (f, g) -> Sub (deriv f x, deriv g x)
		| Mul (f, g) -> Add (Mul (deriv f x, g), Mul (f, deriv g x))
		| Div (f, g) -> Div (Sub (Mul (deriv f x, g), Mul (f, deriv g x)), Mul (g, g))
		| Ln f -> Div (deriv f x, f)
		| Cos f -> Mul (deriv f x, Mul (Sin f, Float (-1.)))
		| Sin f -> Mul (deriv f x, Cos f)
		| Puis (f, g) -> Mul (Puis (f, g), Add (Mul (deriv g x, Ln f), Mul (g, Div (deriv f x, f))))
		| Sqrt f -> Div (deriv f x, Mul (Float 2., Sqrt f))
		| Exp f -> Mul (deriv f x, Exp f)

let rec integrate f x =
	match f with
		| Float f -> Mul (Var x, Float f)
		| Var v when v = x -> Mul (Var x, Var x)
		| Var v -> Var v
		| Add (f, g) -> Add (integrate f x, integrate g x)
		| Sub (f, g) -> Sub (integrate f x, integrate g x)

let is_entier e =
	if e = float_of_int (int_of_float e)
	then true
	else false

let rec calcul_fct f x =
	match f with
  		| Float f -> f
  		| Var v -> x
		| Add (f, g) -> calcul_fct f x +. calcul_fct g x
		| Sub (f, g) -> calcul_fct f x -. calcul_fct g x
		| Mul (f, g) -> calcul_fct f x *. calcul_fct g x
		| Div (f, g) -> calcul_fct f x /. calcul_fct g x
		| Ln f -> log (calcul_fct f x)
		| Cos f -> cos (calcul_fct f x)
		| Sin f -> sin (calcul_fct f x)
		| Puis (f, g) -> if calcul_fct f x > 0. then exp (calcul_fct g x *. log (calcul_fct f x))
						else if calcul_fct f x = 0. then 0.
						else if is_entier (calcul_fct g x) then Util.pow_float (calcul_fct f x) (int_of_float (calcul_fct g x))
						else failwith "calcul_fct: non-real"
		| Sqrt f -> sqrt (calcul_fct f x)
		| Exp f -> exp (calcul_fct f x)


let rec free_variables_present f x =
	match f with
  		| Float f -> false
  		| Var v when v = x-> false
  		| Var _ -> true
		| Add (f, g)
		| Sub (f, g)
		| Mul (f, g)
		| Puis (f, g) 
		| Div (f, g) -> free_variables_present f x || free_variables_present g x
		| Ln f
		| Cos f
		| Sin f
		| Sqrt f
		| Exp f -> free_variables_present f x

let flt f = Float f
let add e1 e2 = Add (e1, e2)
let sub e1 e2 = Sub (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let div e1 e2 = Div (e1, e2)
let puis e1 e2 = Puis (e1, e2)
let neg e = Mul (Float (-1.), e)
let pos e = Mul (Float 1., e)
let cos e = Cos e
let sin e = Sin e
let var v = Var v
let sqrt e = Sqrt e
let expo e = Exp e
let lnp e = Ln e

(*

| Float f -> Float f
| Var x -> Var x
| Add (Float 0., f) -> simp f
| Add (f, Float 0.) -> simp f
| Add (Float f1, Float f2) -> Float (f1 +. f2)

| Add (Float f1, Add (Float f2, f)) -> simp (Add (Float (f1 +. f2), simp f))
| Add (Add (Float f1, f), Float f2) -> simp (Add (Float (f1 +. f2), simp f))
| Add (Float f1, Add (f, Float f2)) -> simp (Add (Float (f1 +. f2), simp f))
| Add (Add (f, Float f1), Float f2) -> simp (Add (Float (f1 +. f2), simp f))

| Add (Add (f, g), Mul (Float f1, i)) when g = i -> Add (simp f, Mul (Float (f1 +. 1.), simp g))
| Add (Add (f, g), Mul (i, Float f1)) when g = i -> Add (simp f, Mul (Float (f1 +. 1.), simp g))
| Add (Add (f, g), Mul (Float f1, i)) when f = i -> Add (simp g, Mul (Float (f1 +. 1.), simp f))
| Add (Add (f, g), Mul (i, Float f1)) when f = i -> Add (simp g, Mul (Float (f1 +. 1.), simp f))
| Add (Mul (Float f1, i), Add (f, g)) when g = i -> Add (simp f, Mul (Float (f1 +. 1.), simp g))
| Add (Mul (i, Float f1), Add (f, g)) when g = i -> Add (simp f, Mul (Float (f1 +. 1.), simp g))
| Add (Mul (Float f1, i), Add (f, g)) when f = i -> Add (simp g, Mul (Float (f1 +. 1.), simp f))
| Add (Mul (i, Float f1), Add (f, g)) when f = i -> Add (simp g, Mul (Float (f1 +. 1.), simp f))

| Add (Add (f, Mul (Float f2, g)), Mul (Float f1, i)) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Add (f, Mul (g, Float f2)), Mul (Float f1, i)) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Add (f, Mul (Float f2, g)), Mul (i, Float f1)) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Add (f, Mul (g, Float f2)), Mul (i, Float f1)) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Add (Mul (Float f2, f), g), Mul (Float f1, i)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))
| Add (Add (Mul (f, Float f2), g), Mul (Float f1, i)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))
| Add (Add (Mul (Float f2, f), g), Mul (i, Float f1)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))
| Add (Add (Mul (f, Float f2), g), Mul (i, Float f1)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))

| Add (Mul (Float f1, i), Add (f, Mul (Float f2, g))) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Mul (Float f1, i), Add (f, Mul (g, Float f2))) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Mul (i, Float f1), Add (f, Mul (Float f2, g))) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Mul (i, Float f1), Add (f, Mul (g, Float f2))) when g = i -> Add (simp f, Mul (Float (f1 +. f2), simp g))
| Add (Mul (Float f1, i), Add (Mul (Float f2, f), g)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))
| Add (Mul (Float f1, i), Add (Mul (f, Float f2), g)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))
| Add (Mul (i, Float f1), Add (Mul (Float f2, f), g)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))
| Add (Mul (i, Float f1), Add (Mul (f, Float f2), g)) when f = i -> Add (simp g, Mul (Float (f1 +. f2), simp f))

| Add (f, Mul (Float f1, g)) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))
| Add (Mul (Float f1, f), g) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))
| Add (f, Mul (g, Float f1)) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))
| Add (Mul (g, Float f1), f) when f = g -> simp (Mul (Float (f1 +. 1.), simp f))

| Add (Mul (f, Float f1), Mul (g, Float f2)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))
| Add (Mul (Float f1, f), Mul (Float f2, g)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))
| Add (Mul (f, Float f1), Mul (g, Float f2)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))
| Add (Mul (f, Float f1), Mul (Float f2, g)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))
| Add (Mul (Float f1, f), Mul (g, Float f2)) when f = g -> simp (Mul (Float (f1 +. f2), simp f))

| Add (f, g) when f = g -> simp (Mul (Float 2., simp f))
| Add (f, g) -> Add (simp f, simp g)

| Sub (Float 0., f) -> simp (Mul (Float (-1.), simp f))
| Sub (f, Float 0.) -> simp f
| Sub (Float f1, Float f2) -> Float (f1 -. f2)
| Sub (f, g) when f = g -> Float 0.
| Sub (f, g) -> Sub (simp f, simp g)

| Div (Float 0., f) -> Float 0.
| Div (f, Float 1.) -> simp f
| Div (Float f1, Float f2) -> Float (f1 /. f2)
| Div (f, g) when f = g -> Float 1.
| Div (f, g) -> Div (simp f, simp g)

| Mul (Float 1., f) -> simp f
| Mul (f, Float 1.) -> simp f
| Mul (Float 0., f) -> Float 0.
| Mul (f, Float 0.) -> Float 0.
| Mul (Float f1, Float f2) -> Float (f1 *. f2)
| Mul (Float f1, Mul (Float f2, f)) -> simp (Mul (Float (f1 *. f2), simp f))
| Mul (Float f1, Mul (f, Float f2)) -> simp (Mul (Float (f1 *. f2), simp f))
| Mul (Mul (Float f2, f), Float f1) -> simp (Mul (Float (f1 *. f2), simp f))
| Mul (Mul (f, Float f2), Float f1) -> simp (Mul (Float (f1 *. f2), simp f))
| Mul (Puis (f, g), h) when f = h -> Puis (simp f, simp (Add (simp g, Float 1.)))
| Mul (f, g) when f = g -> Puis (simp f, Float 2.)
| Mul (f, g) -> Mul (simp f, simp g)

| Puis (f, Float 0.) -> Float 1.
| Puis (Float 0., f) -> Float 0.
| Puis (f, Float 1.) -> simp f
| Puis (f, g) -> Puis (simp f, simp g)

| Ln f -> Ln (simp f)
| Cos f -> Cos (simp f)
| Sin f -> Sin (simp f)
| Sqrt f -> Sqrt (simp f)
| Exp f -> Exp (simp f)

*)