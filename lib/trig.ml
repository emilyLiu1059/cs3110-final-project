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

let arcsin x =
  x
  +. ((x ** 3.) /. 6.)
  +. ((x ** 5.) *. 3. /. 40.)
  +. ((x ** 7.) *. 5. /. 112.)
  +. ((x ** 9.) *. 35. /. 1152.)

let arccos x =
  (Float.pi /. 2.)
  -. (x
     +. ((x ** 3.) /. 6.)
     +. ((x ** 5.) *. 3. /. 40.)
     +. ((x ** 7.) *. 5. /. 112.)
     +. ((x ** 9.) *. 35. /. 1152.))

let arctan x =
  let max_iterations = 20 in
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
  aux 0 0.0
