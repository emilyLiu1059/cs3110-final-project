let deg_to_rad x = x *. (Float.pi /. 180.0)

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
  let x = deg_to_rad (float_of_int x) in
  sin_helper 0.0 0.0 x

let rec cos_helper i acc value =
  if i = 10.0 then acc
  else
    let taylor =
      (-1.0 ** i) *. (value ** (2.0 *. i)) /. factorial (2.0 *. i) 1.0
    in
    cos_helper (i +. 1.0) (acc +. taylor) value

let cos x =
  let x = deg_to_rad (float_of_int x) in
  cos_helper 0.0 0.0 x

let tan x = sin x /. cos x
