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
	| Puis of float * formel
	| Sqrt of formel
	| Exp of formel

let rec simplify f x =
	let rec simplify_var f x =
		match f with
			| Float f -> 0.
			| Var v when v = x -> 1.
			| Var v -> 0.
			| Add (f, g) ->	simplify_var f x +. simplify_var g x
	in
Mul (Float (simplify_var f x), Var x)

let rec deriv f x =
	match f with
  		| Float f -> Float 0.
  		| Var v when v = x -> Float 1.
  		| Var v -> Float 0.
		| Add (f, g) ->	Add (deriv f x, deriv g x)
		| Sub (f, g) -> Sub (deriv f x, deriv g x)
		| Mul (f, g) -> Add (Mul (f, deriv g x), Mul (g, deriv f x))
		| Div (f, g) -> Div (Sub (Mul (deriv f x, g), Mul (f, deriv g x)), Mul (g, g))
		| Ln f -> Div (deriv f x, f)
		| Cos f -> Mul (deriv f x, Mul (Sin f, Float (-1.)))
		| Sin f -> Mul (deriv f x, Cos f)
		| Puis (n, f) -> Mul (deriv f x, Mul (Float n, Puis (n -. 1., f)))
		| Sqrt f -> Div (deriv f x, Mul (Float 2., Sqrt f))
		| Exp f -> Mul (deriv f x, Exp f)

let rec integrate f x =
	match f with
		| Float f -> Mul (Var x, Float f)
		| Var v when v = x -> Mul (Var x, Var x)
		| Var v -> Var v
		| Add (f, g) -> Add (integrate f x, integrate g x)
		| Sub (f, g) -> Sub (integrate f x, integrate g x)
		(*
		| Mul (f, g) -> integrate (Add (integrate (Mul (deriv f x, g)) x, integrate (Mul (f, deriv g x)) x)) x
		*)

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
		| Puis (n, f) -> calcul_fct (Exp (Mul (Float n, Ln (f)))) x
		| Sqrt f -> sqrt (calcul_fct f x)
		| Exp f -> exp (calcul_fct f x)

let rec string_fct f =
	match f with
  		| Float f -> string_of_float f
  		| Var v -> "x"
		| Add (f, g) -> string_fct f ^ " + " ^ string_fct g
		| Sub (f, g) -> string_fct f ^ " - " ^ string_fct g
		| Mul (f, g) -> string_fct f ^ " * " ^ string_fct g
		| Div (f, g) -> string_fct f ^ " / " ^ string_fct g
		| Ln f -> "ln (" ^ string_fct f ^ ")"
		| Cos f -> "cos (" ^ string_fct f ^ ")"
		| Sin f -> "sin (" ^ string_fct f ^ ")"
		| Puis (n, f) -> "(" ^ string_fct f ^ ")" ^ string_of_float n
		| Sqrt f -> "sqrt (" ^ string_fct f ^ ")"
		| Exp f -> "exp (" ^ string_fct f ^ ")"

let rec free_variables_present f x =
	match f with
  		| Float f -> false
  		| Var v when v = x-> false
  		| Var _ -> true
		| Add (f, g)
		| Sub (f, g)
		| Mul (f, g)
		| Div (f, g) -> free_variables_present f x || free_variables_present g x
		| Ln f
		| Cos f
		| Sin f
		| Puis (_, f)
		| Sqrt f
		| Exp f -> free_variables_present f x


let flt f = Float f
let add e1 e2 = Add (e1, e2)
let sub e1 e2 = Sub (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let div e1 e2 = Div (e1, e2)
let neg e = Mul (Float (-1.), e)
let pos e = Mul (Float 1., e)
let cos e = Cos e
let sin e = Sin e
let var v = Var v
let sqrt e = Sqrt e
let exp e = Exp e
let lnp e = Ln e
	
