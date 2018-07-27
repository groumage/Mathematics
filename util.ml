let rec pow_float a b =
	match b with
	| 1 -> a
	| _ -> a *. pow_float a (b-1)

let abs_float a =
	if a > 0. then a else -.a