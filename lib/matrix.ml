let add a b =
  let rows = Array.length a in
  let cols = Array.length a.(0) in
  let result = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- a.(i).(j) +. b.(i).(j)
    done
  done;
  result

let subtract a b =
  let rows = Array.length a in
  let cols = Array.length a.(0) in
  let result = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- a.(i).(j) -. b.(i).(j)
    done
  done;
  result

let transpose (matrix : float array array) : float array array =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let transposed = Array.make_matrix cols rows 0. in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      transposed.(j).(i) <- matrix.(i).(j)
    done
  done;
  transposed

let multiply a b =
  let rows_a = Array.length a in
  let cols_a = Array.length a.(0) in
  let cols_b = Array.length b.(0) in
  let result = Array.make_matrix rows_a cols_b 0.0 in
  let transposed_b = transpose b in
  for i = 0 to rows_a - 1 do
    for j = 0 to cols_b - 1 do
      let sum = ref 0.0 in
      for k = 0 to cols_a - 1 do
        sum := !sum +. (a.(i).(k) *. transposed_b.(j).(k))
      done;
      result.(i).(j) <- !sum
    done
  done;
  result

let to_string arr =
  let rows = Array.length arr in
  let cols = Array.length arr.(0) in
  let str_builder = ref "" in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      str_builder := !str_builder ^ string_of_float arr.(i).(j) ^ " "
    done;
    str_builder := !str_builder ^ "\n"
  done;
  !str_builder
