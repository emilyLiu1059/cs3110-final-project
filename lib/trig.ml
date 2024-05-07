let deg_to_rad x = x *. (Float.pi /. 180.0)
let rad_to_deg x = x *. (180.0 /. Float.pi)

let rec factorial x acc =
  if x = 0.0 then acc else factorial (x -. 1.0) (acc *. x)

let rec sin_helper i acc value =
  if i = 10.0 then acc
  else
    let taylor =
      (-1.0 ** i)
      *. (value ** ((2.0 *. i) +. 1.0))
      /. factorial ((2.0 *. i) +. 1.0) 1.0
    in
    sin_helper (i +. 1.0) (acc +. taylor) value

let sin x =
  let x = deg_to_rad x in
  sin_helper 0.0 0.0 x

let rec cos_helper i acc value =
  if i = 10.0 then acc
  else
    let taylor =
      (-1.0 ** i) *. (value ** (2.0 *. i)) /. factorial (2.0 *. i) 1.0
    in
    cos_helper (i +. 1.0) (acc +. taylor) value

let cos x =
  let x = deg_to_rad x in
  cos_helper 0.0 0.0 x

let tan x = sin x /. cos x
let csc x = 1. /. sin x
let sec x = 1. /. cos x
let cot x = 1. /. tan x

let arctan x =
  let max_iterations = 100 in
  let rec aux n acc =
    if n >= max_iterations then acc
    else
      let term =
        (-1.0 ** float_of_int n)
        *. (x ** ((2.0 *. float_of_int n) +. 1.0))
        /. ((2.0 *. float_of_int n) +. 1.0)
      in
      aux (n + 1) (acc +. term)
  in
  rad_to_deg (aux 0 0.0)

let arcsin_helper x = x /. Float.sqrt (1. -. (x *. x))
let arcsin x = if x = 1. then 90. else arctan (arcsin_helper x)

let arccos x =
  if x = 1. then 0. else rad_to_deg ((Float.pi /. 2.) -. deg_to_rad (arcsin x))
