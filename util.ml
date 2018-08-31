(****************************************
 * General-purpose utility functions
 ****************************************)
let get_first_of_list lst =
  match lst with
    []    -> failwith "Empty list"
  | x::xs -> x;;

let get_rest_of_list lst =
  match lst with
    []    -> failwith "Empty list"
  | x::xs -> xs;;

let get_last_of_list lst =
  let rec get_last_of_list_helper so_far lst =
    match lst with
      []    -> so_far
    | x::xs -> get_last_of_list_helper x xs
  in match lst with
    []    -> failwith "Empty list"
  | x::xs -> get_last_of_list_helper x xs;;

(* Determine if fraction portion of float is 0 *)
let is_float_int f = float_of_int (int_of_float f) = f;;

(* Determine if integer is even or odd *)
let is_even i =
  let q = i / 2 in
  let r = i - q * 2 in
  r = 0;;

(* Compute a^b via a^b = exp(b * log a).  Note that log a is only
   defined if a > 0, so there are special cases for a = 0 and a < 0 *)

let power a b =
  if a > 0. then
    exp (b *. log a)
  else if a = 0. then
    0.
  else if is_float_int b then
    let i = int_of_float b in
    let p = exp (b *. log (-.a)) in   (* compute (-a)^b *)
    if is_even i then p else -.p      (* now fix sign *)
  else
    failwith ("Cannot raise negative number " ^ string_of_float a ^ " to non-integer power " ^ string_of_float b);;

(* Euclidean GCD (greatest common denominator) algorithm - works on ints *)
let rec gcd a b =
  let q = a / b in
  let r = a - b * q in
  if r = 0 then abs b else gcd b r;;

let list_contains lst i = List.fold_left (||) false (List.map (fun x -> x = i) lst);;
let intersect_list a b  = List.fold_right (@) (List.map (fun x -> if list_contains a x then [x] else []) b) [];;
let intersect_diff a b  = List.fold_right (@) (List.map (fun x -> if list_contains a x then [] else [x]) b) [];;

let abs_float a =
  if a > 0. then
    a
  else -.a

let rec list_length l =
  match l with
    [] -> 0
  | h :: t -> 1 + list_length t

let rec get_list l i =
  match l with
    [] -> failwith "unbound"
  | h :: t -> if i = 1 then h else get_list t (i-1)