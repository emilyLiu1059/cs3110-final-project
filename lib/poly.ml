let pi = 3.141592653
let e = 2.718281828
let rad_to_deg rad = rad *. 180. /. pi
let deg_to_rad deg = deg *. pi /. 180.

(* [c0; c1; ...; cn] represents the polynomial c0 + c1 x + ... + cn x^n The
   empty list represents 0 a list of 0s also represents 0*)

let rec eval x = function
  | [] -> 0.
  | c :: rest -> c +. (x *. eval x rest)

let rec deri_once = function
  | [] -> []
  | _ :: [] -> []
  | h :: rest ->
      let x = rest |> List.length |> float_of_int in
      (x *. h) :: deri_once rest

let rec deri n poly =
  if n = 1 then deri_once poly else deri (n - 1) (deri_once poly)

let rec inte_once c = function
  | [] -> []
  | h :: [] -> [ h; c ]
  | h :: rest ->
      let x = rest |> List.length |> float_of_int in
      (h /. x) :: inte_once c rest

let rec add p1 p2 =
  match (p1, p2) with
  | [], _ -> p2 (* empty list is the 0 polynomial *)
  | _, [] -> p1 (* empty list is the 0 polynomial *)
  | c1 :: p1, c2 :: p2 -> (c1 +. c2) :: add p1 p2

let degree p =
  (* [drop_while p l] is [l] with its longest prefix satisfying
   * [p] is removed. We use this to remove leading zeros from the
   * coefficient list.
   *)
  let rec drop_while f = function
    | [] -> []
    | x :: xs -> if f x then drop_while f xs else x :: xs
  in
  List.length (drop_while (fun x -> x = 0.) (List.rev p)) - 1

let multiply p1 p2 =
  let rec tabulate f s = function
    | n when n <= 0 -> []
    | n -> f s :: tabulate f (s + 1) (n - 1)
  in

  let rec take l n =
    match (l, n) with
    | _, 0 -> []
    | [], n -> 0.0 :: take [] (n - 1)
    | x :: xs, n -> x :: take xs (n - 1)
  in

  let rec dotProduct l1 l2 =
    match (l1, l2) with
    | [], _ -> 0.0
    | _, [] -> 0.0
    | x1 :: l1, x2 :: l2 -> (x1 *. x2) +. dotProduct l1 l2
  in

  let ith i =
    let p1_up_to_ith_power = take p1 (i + 1) in
    let p2_up_to_ith_power = take p2 (i + 1) in
    dotProduct p1_up_to_ith_power (List.rev p2_up_to_ith_power)
  in

  (* Assuming the 'degree' function is defined for float lists *)
  tabulate ith 0 (degree p1 + degree p2 + 1)

(* You need to define or adjust the 'degree' function for float lists. *)
let degree p =
  let rec aux d = function
    | [] -> d
    | 0.0 :: xs -> aux (d - 1) xs
    | _ :: xs -> aux d xs
  in
  aux (List.length p - 1) (List.rev p)

let string_of_float_list lst =
  let string_list = List.map string_of_float lst in
  String.concat ", " string_list

let poly_to_string p =
  let rec aux i = function
    | [] -> ""
    | h :: t -> (
        let term =
          match (h, i) with
          | 0., _ -> ""
          | h, 0 -> string_of_float h
          | h, 1 ->
              if h = 1. then "x"
              else if h = -1. then "-x"
              else string_of_float h ^ "x"
          | h, i ->
              if h = 1. then "x^" ^ string_of_int i
              else if h = -1. then "-x^" ^ string_of_int i
              else string_of_float h ^ "x^" ^ string_of_int i
        in
        if term = "" then aux (i + 1) t
        else
          match aux (i + 1) t with
          | "" -> term
          | s -> term ^ " + " ^ s)
  in
  aux 0 (List.rev p)

(* Using this function, you can convert a polynomial like [1.; -3.; 2.] which
   represents 1 - 3x + 2x^2 into a string "2x^2 - 3x + 1". *)

let divide_polynomials dividend divisor =
  let rec div_aux dividend divisor lead_coeff_divisor deg_divisor result =
    if List.length dividend < deg_divisor then (result, dividend)
    else
      let lead_coeff = List.hd (List.rev dividend) in
      let factor = lead_coeff /. lead_coeff_divisor in
      let deg_diff = List.length dividend - deg_divisor in
      let subtrahend =
        List.map
          (fun x -> x *. factor)
          (divisor @ List.init deg_diff (fun _ -> 0.))
      in
      let new_dividend = List.map2 (fun x y -> x -. y) dividend subtrahend in
      let new_result = (factor :: List.init deg_diff (fun _ -> 0.)) @ result in
      div_aux
        (List.rev (List.tl (List.rev new_dividend)))
        divisor lead_coeff_divisor deg_divisor new_result
  in
  div_aux dividend divisor (List.hd (List.rev divisor)) (List.length divisor) []

let rec gcd_polynomial p1 p2 =
  if p2 = [] then p1 (* p2 is zero *)
  else
    let _, remainder = divide_polynomials p1 p2 in
    gcd_polynomial p2 remainder

let compose_polynomials p g =
  List.fold_right (fun coeff acc -> add (multiply acc g) [ coeff ]) p [ 1.0 ]
(* Starts with the constant term and composes upwards *)

let rec subtract p1 p2 =
  match (p1, p2) with
  | [], _ -> List.map (fun x -> -.x) p2
  | _, [] -> p1
  | c1 :: p1', c2 :: p2' -> (c1 -. c2) :: subtract p1' p2'

let eval_at_points poly points = List.map (fun x -> eval x poly) points
