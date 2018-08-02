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

let rec simplify f =
	let f_simplify = simp f in
	if f_simplify = f 
	then f_simplify 
	else simplify f_simplify
	and simp f = 
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Add (Float 0., f) -> simp f
			| Add (f, Float 0.) -> simp f
			| Add (Float f1, Float f2) -> Float (f1 +. f2)
			| Add (f, Mul (Float f1, g)) when f = g -> simp (Mul (Float (f1 +. 1.), f))
			| Add (f, Mul (g, Float f1)) when f = g -> simp (Mul (Float (f1 +. 1.), f))
			| Add (Mul (Float f1, f), g) when f = g -> simp (Mul (Float (f1 +. 1.), f))
			| Add (Mul (f, Float f1), g) when f = g -> simp (Mul (Float (f1 +. 1.), f))
			| Add (f, g) when f = g -> simp (Mul (Float 2., f))
			| Add (f, g) -> Add (simp f, simp g)
			| Sub (Float 0., f) -> simp (Mul (Float (-1.), f))
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
			| Mul (Puis (f, g), h) when f = h -> Puis (simp f, simp (Add (g, Float 1.)))
			| Mul (f, g) when f = g -> simp (Puis (simp f, Float 2.))
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
let is_float f =
	match f with
		| Float f -> true
		| _ -> false

let get_float f =
	match f with
		| Float f -> f
		| _ -> failwith "get_float: a float is expected"

let is_var v =
	match v with
		| Var v -> true
		| _ -> false

let get_var v =
	match v with
		| Var v -> v
		| _ -> failwith "get_var: a var is expected"*)
			(*
(*
let rec simplify f x =
	let rec simplify_var f x =
		match f with
			| Float f -> 0.
			| Var v when v = x -> 1.
			| Var v -> 0.
			| Add (f, g) ->	simplify_var f x +. simplify_var g x
	in
Mul (Float (simplify_var f x), Var x)
*)
	(*let rec is_zero f =
		match f with
			| Float f when f = 0. -> true
			| Float f -> false
			| Var x -> false
			| Add (f, g) -> is_zero f && is_zero g
			| Sub (f, g) -> is_zero f && is_zero g
			| Mul (f, g) -> is_zero f || is_zero g
			| Div (f, g) -> is_zero f
			| _ -> false
	in
	let rec simplify_zero f =
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Add (f, g) -> begin
								if is_zero f && is_zero g
								then Float 0.
								else if is_zero f && is_zero g == false
								then simplify_zero g
								else if is_zero f == false && is_zero g
								then simplify_zero f
								else Add (simplify_zero f, simplify_zero g)
							end
			| Sub (f, g) -> begin
								if is_zero f && is_zero g
								then Float 0.
								else if is_zero f && is_zero g == false
								then simplify_zero g
								else if is_zero f == false && is_zero g
								then simplify_zero f
								else Sub (simplify_zero f, simplify_zero g)
							end
			| Mul (f, g) -> begin
								if is_zero f || is_zero g
								then Float 0.
								else Mul (simplify_zero f, simplify_zero g)
							end
			| Div (f, g) -> begin
								if is_zero f
								then Float 0.
								else Div (simplify_zero f, simplify_zero g)
							end
			| Puis (f, g) -> begin
								if is_zero f
								then Float 0.
								else if is_zero g
								then Float 1.
								else Puis (simplify_zero f, simplify_zero g)
							end
			| Cos f -> Cos (simplify_zero f)
			| Sin f -> Sin (simplify_zero f)
	in
	let rec exist_var f =
		match f with
			| Float f -> false
			| Var x -> true
			| Mul (f, g) -> exist_var f || exist_var g
			| Add (f, g) -> exist_var f || exist_var g
	in
	let rec simplify_op f =
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Mul (f, g) -> simplify_mul (Mul (simplify_op f, simplify_op g))
			| Add (f, g) -> simplify_add (Add (simplify_op f, simplify_op g))
			| Sub (f, g) -> simplify_sub (Sub (simplify_op f, simplify_op g))
			| Puis (f, g) -> simplify_puis (Puis (simplify_op f, simplify_op g))
			| Cos f -> Cos (simplify_op f)
			| Sin f -> Sin (simplify_op f)
	and
	simplify_add f =
		match f with
			| Add (Float f, Float g) -> Float (f +. g)
			| Add (Var x, Var y) -> Mul (Float 2., Var x)
			| Add (f, g) -> Add (simplify_op f, simplify_op g)
	and
	simplify_sub f =
		match f with
			| Sub (Float f, Float g) -> Float (f -. g)
			| Sub (Var x, Var y) -> Float 0.
			| Sub (f, g) -> Sub (simplify_op f, simplify_op g)
	and
	simplify_mul f =
		match f with
			| Mul (Float f, Float g) -> Float (f *. g)
			| Mul (f, g) -> Mul (simplify_op f, simplify_op g)
	and
	simplify_puis f =
		match f with
			| Puis (Float f, g) -> Puis (Float f, simplify_op g)
			| Puis (f, g) -> Puis (simplify_op f, simplify_op g)
	in*)
			| Mul (f, g) when is_float f && is_var g -> Mul (Float (get_float f), Var (get_var g))
			| Mul (f, g) when is_var f && is_float g -> Mul (Float (get_float g), Var (get_var f))
			| Add (f, g) when is_var f && is_var g -> Mul (Float 2., Var (get_var f))
			*)
	(*let rec calcul f res =
		match f with
			| Float f -> Float f
			| Var x -> Mul (Float res, Var x)
			| Add (f, g) -> begin
								if is_float f && is_float g
								then Float (get_float f +. get_float g)
								else if is_float f == false && is_float g
								then calcul f (res +. get_float g)
								else if is_float f && is_float g == false
								then calcul g (res +. get_float f)
								else Add (calcul f res, calcul g res)
							end
			| Sub (f, g) -> begin
								if is_float f && is_float g
								then Float (get_float f -. get_float g)
								else if is_float f == false && is_float g
								then calcul f (res -. get_float g)
								else if is_float f && is_float g == false
								then calcul g (res -. get_float f)
								else Sub (calcul f res, calcul g res)
							end
			| Mul (f, g) -> begin
								if is_float f && is_float g
								then Float (get_float f *. get_float g)
								else if is_float f == false && is_float g
								then calcul f (res *. get_float g)
								else if is_float f && is_float g == false
								then calcul g (res *. get_float f)
								else Mul (calcul f res, calcul g res)
							end
			| Cos f -> Mul (Float res, Cos f)
			| Sin f -> Mul (Float res, Sin f)
	in
	let rec calcul_float f =
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Mul (f, g) when is_float f -> calcul g (get_float f)
			| Mul (f, g) when is_float g -> calcul f (get_float g)
			| Mul (f, g) -> Mul (calcul_float f, calcul_float g)
			| Add (f, g) when is_float f -> calcul g (get_float f)
			| Add (f, g) when is_float g -> calcul f (get_float g)
			| Add (f, g) -> Add ((calcul_float f), (calcul_float g))
			| Sub (f, g) when is_float f -> calcul g (get_float f)
			| Sub (f, g) when is_float g -> calcul f (get_float g)
			| Sub (f, g) -> Sub (calcul_float f, calcul_float g)
	in*)
			(*| Mul (Float f, Cos c) -> calcul (Cos c) f
			| Mul (Float f, Cos c) -> calcul (Cos c) f
			| Add (f, g) -> Add (calcul_float g, calcul_float f)
			| Mul (Float f, Sin s) -> calcul (Sin s) f
			| Mul (Float f, Sin s) -> calcul (Sin s) f
			| Mul (f, g) -> calcul (Mul (f, g)) 2.*)
	(*let rec bidule f =
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Mul (f, g) when is_float f && is_float g -> Float (get_float f *. get_float g)
			| Add (f, g) when is_float f && is_float g -> Float (get_float f +. get_float g)
			| Mul (f, g) -> bidule (Mul (bidule f, bidule g))
			| Add (f, g) -> bidule (Add (bidule f, bidule g))*)
		(*
		| Mul (f, g) -> integrate (Add (integrate (Mul (deriv f x, g)) x, integrate (Mul (f, deriv g x)) x)) x
		*)

	(*let rec prod f =
		match f with
			| Float f -> f
			| Var x -> 1.
			| Mul (f, g) -> prod f *. prod g
			| _ -> 1.
	in
	let rec calcul f =
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Mul (f, g) -> Mul (prod f *. prod g, *)
	(*let rec is_one f =
		match f with
			| Float f when f = 1. -> true
			| Float f -> false
			| Var x -> false
			| Add (f, g) -> are_one f g +.
			| Sub (f, g) -> are_one f g -.
			| Mul (f, g) -> are_one f g *.
			| Div (f, g) -> are_one f g /.
	and
	are_one f g op =
		match (f, g) with
			| (Float f1, Float f2) when f1 op f2 = 1. -> true
			| (Float f1, Float f2) -> false
			| (Var x, _) -> false
			| (_, Var x) -> false
			| (Add (f, g), Add (h, i)) -> are_one (Add (f, g)) (Add (h, i)) +.
	in*)
	(*let rec is_plus_minus_one f j =
		match f with
			| Float f when f = j -> true
			| Mul (f, g) -> is_plus_minus_one f j && is_plus_minus_one g j
			| _ -> false
	and
	are_plus_minus_one f g j =
		match (f, g) with
			| (Float f1, Float f2) when f1 +. f2 = j -> true
			| (Float f1, Float f2) -> false
			| (Var x, _) -> false
			| (_, Var x) -> false
			| (Add (f, g), Add (h, i)) -> are_plus_minus_one (Add (f, g)) (Add (h, i)) j
			| (Sub (f, g), Sub (h, i)) -> are_plus_minus_one (Sub (f, g)) (Sub (h, i)) j
			| (Add (f, g), Sub (h, i)) -> are_plus_minus_one (Add (f, g)) (Sub (h, i)) j
			| (Sub (f, g), Add (h, i)) -> are_plus_minus_one (Sub (f, g)) (Add (h, i)) j
			| (Mul (f, g), Mul (h, i)) -> are_plus_minus_one (Mul (f, g)) (Mul (h, i)) j
			| _ -> false
	in
	let rec count_minus f =
		match f with
			| Float f when f = -1. -> 1
			| Var x -> 0
			| Mul (f, g) -> count_minus f + count_minus g
			| _ -> 0
	in
	let rec simplify_one f =
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Add (f, g) -> begin
								if is_plus_minus_one f 1. && is_plus_minus_one g 1.
								then Float 2.
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.)
								then Float (-2.)
								else if is_plus_minus_one f (1.) && is_plus_minus_one g (-1.)
								then Float 0.
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1.
								then Float 0.
								else if is_plus_minus_one f 1. && is_plus_minus_one g 1. == false
								then Add (Float 1., simplify_one g)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1. == false
								then Add (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. && is_plus_minus_one g (-1.) == false
								then Add (Float 1., simplify_one g)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.) == false
								then Add (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g 1.
								then Add (simplify_one f, Float 1.)
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g (-1.)
								then Add (simplify_one f, Float (-1.))
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g 1.
								then Add (simplify_one f, Float 1.)
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g (-1.)
								then Add (simplify_one f, Float (-1.))
								else Add (simplify_one f, simplify_one g)
							end
			| Sub (f, g) -> begin
								if is_plus_minus_one f 1. && is_plus_minus_one g 1.
								then Float 0.
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.)
								then Float 0.
								else if is_plus_minus_one f (1.) && is_plus_minus_one g (-1.)
								then Float 2.
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1.
								then Float (-2.)
								else if is_plus_minus_one f 1. && is_plus_minus_one g 1. == false
								then Sub (Float 1., simplify_one g)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1. == false
								then Sub (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. && is_plus_minus_one g (-1.) == false
								then Sub (Float 1., simplify_one g)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.) == false
								then Sub (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g 1.
								then Sub (simplify_one f, Float 1.)
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g (-1.)
								then Sub (simplify_one f, Float (-1.))
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g 1.
								then Sub (simplify_one f, Float 1.)
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g (-1.)
								then Sub (simplify_one f, Float (-1.))
								else Sub (simplify_one f, simplify_one g)
							end
			| Mul (f, g) -> begin
								print_string (string_of_bool (is_plus_minus_one f (-1.)));
								print_string "\n";
								print_string (string_fct f);
								print_string "\n";
								print_string (string_of_bool (is_plus_minus_one g (-1.)));
								print_string "\n";
								print_string (string_fct g);
								print_string "\n\n";
								if is_plus_minus_one f 1. && is_plus_minus_one g 1.
								then Float 1.
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.)
								then Float 1.
								else if is_plus_minus_one f (1.) && is_plus_minus_one g (-1.)
								then Float (-1.)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1.
								then Float (-1.)
								else if is_plus_minus_one f 1. && is_plus_minus_one g 1. == false
								then simplify_one g
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1. == false
								then Mul (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. && is_plus_minus_one g (-1.) == false
								then simplify_one g
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.) == false
								then Mul (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g 1.
								then simplify_one f
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g (-1.)
								then Mul (simplify_one f, Float (-1.))
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g 1.
								then simplify_one f
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g (-1.)
								then Mul (simplify_one f, Float (-1.))
								else Mul (simplify_one f, simplify_one g)
							end
			| Div (f, g) -> begin
								if is_plus_minus_one f 1. && is_plus_minus_one g 1.
								then Float 1.
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.)
								then Float 1.
								else if is_plus_minus_one f (1.) && is_plus_minus_one g (-1.)
								then Float (-1.)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1.
								then Float (-1.)
								else if is_plus_minus_one f 1. && is_plus_minus_one g 1. == false
								then Div (Float 1., simplify_one g)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g 1. == false
								then Div (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. && is_plus_minus_one g (-1.) == false
								then Div (Float 1., simplify_one g)
								else if is_plus_minus_one f (-1.) && is_plus_minus_one g (-1.) == false
								then Div (Float (-1.), simplify_one g)
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g 1.
								then simplify_one f
								else if is_plus_minus_one f 1. == false && is_plus_minus_one g (-1.)
								then Mul (simplify_one f, Float (-1.))
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g 1.
								then simplify_one f
								else if is_plus_minus_one f (-1.) == false && is_plus_minus_one g (-1.)
								then Mul (simplify_one f, Float (-1.))
								else Div (simplify_one f, simplify_one g)
							end
			| Cos f -> Cos (simplify_one f)
			| Sin f -> Sin (simplify_one f)
	in*)
	(*let rec count_minus_plus f =
		match f with
			| Float f when f < 0. -> 1
			| Float f when f >= 0. -> 0
			| Var x -> 0
			| Add (f, g) -> 0
			| Sub (f, g) -> 0
			| Mul (f, g) -> count_minus_plus f + count_minus_plus g
			| Cos f -> 0
			| Sin f -> 0
	in
	let rec delete_minus_plus f =
		match f with
			| Float f when (f = (-1.) || f = 1.) -> Float 1.
			| Float f -> Float f
			| Var x -> Var x
			| Add (f, g) -> Add (delete_minus_plus f, delete_minus_plus g)
			| Sub (f, g) -> Sub (delete_minus_plus f, delete_minus_plus g)
			| Mul (f, g) -> Mul (delete_minus_plus f, delete_minus_plus g)
			| Div (f, g) -> Div (delete_minus_plus f, delete_minus_plus g)
			| Cos f -> Cos (delete_minus_plus f)
			| Sin f -> Sin (delete_minus_plus f)
	in
	let rec simplify_minus_plus f =
		match f with
			| Float f -> Float f
			| Var x -> Var x
			| Add (f, g) -> Add (simplify_minus_plus f, simplify_minus_plus g)
			| Sub (f, g) -> Sub (simplify_minus_plus f, simplify_minus_plus g)
			| Mul (f, g) -> if count_minus_plus (Mul (f, g)) mod 2 == 0
							then Mul (Float (1.), Mul (delete_minus_plus f, delete_minus_plus g))
							else Mul (Float (1.), Mul (delete_minus_plus f, delete_minus_plus g))
			| Cos f -> Cos (simplify_minus_plus f)
			| Sin f -> Sin (simplify_minus_plus f)
	in*)
