let deg_to_rad x = x *. (Float.pi /. 180.0)

let rec factorial x acc = if x = 0 then acc else factorial (x - 1) (acc *. x)

let rec sin_helper i acc value = 
  if i = 10 then acc 
  else let taylor = (((-1.0) ** float_of_int i) * (value ** (2.0 * i + 1))) /. float_of_int (factorial (2 * i + 1) 1) in 
  sin_helper (i + 1) (acc +. taylor)

let sin x = let x = deg_to_rad x in 
  sin_helper 0 0.0 x

let cos_helper i acc value = 
  if i = 10 then acc 
  else let taylor = (((-1.0) ** float_of_int i) * (value ** (2.0 * i))) /. float_of_int (factorial (2 * i) 1) in 
  cos_helper (i + 1) (acc +. taylor)

let cos x = let x = deg_to_rad x in 
cos_helper 0 0.0 x

let rec tan_helper i acc =
  if i = 10 then acc
  else
    let sign = if i mod 2 = 0 then 1.0 else -1.0 in
    let term = sign *. (x ** (2.0 *. float_of_int i +. 1.0)) /. (2.0 *. float_of_int i +. 1.0) in
    tan_helper (i + 1) (acc +. term)

let tan x =
  let x = deg_to_rad x in
  tan_helper 0 x 