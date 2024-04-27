let add x y = x +. y
let subtract x y = x -. y
let multiply x y = x *. y
let divide x y = x /. y

let rec pow base exp =
  if exp = 0 then 1. else if exp = 1 then base else base *. pow base (exp - 1)

let nth_root (x : float) n =
  let rec find_root x0 =
    let xn =
      ((float_of_int (n - 1) *. x0) +. (x /. (x0 ** float_of_int (n - 1))))
      /. float_of_int n
    in
    if abs_float (xn -. x0) < 0.0001 then xn else find_root xn
  in
  if x = 0.0 then 0.0 else if n = 1 then x else find_root 1.0

let abs (x : float) = if x < 0. then -.x else x
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
