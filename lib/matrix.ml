let add a b =
  if
    Array.length a <> Array.length b || Array.length a.(0) <> Array.length b.(0)
  then invalid_arg "Unmatched dimensions";
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
  if
    Array.length a <> Array.length b || Array.length a.(0) <> Array.length b.(0)
  then invalid_arg "Unmatched dimensions";
  let rows = Array.length a in
  let cols = Array.length a.(0) in
  let result = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- a.(i).(j) -. b.(i).(j)
    done
  done;
  result

let transpose a =
  let rows = Array.length a in
  let cols = Array.length a.(0) in
  let transposed = Array.make_matrix cols rows 0. in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      transposed.(j).(i) <- a.(i).(j)
    done
  done;
  transposed

let multiply a b =
  let rows_a = Array.length a in
  let cols_a = Array.length a.(0) in
  let rows_b = Array.length b in
  let cols_b = Array.length b.(0) in
  let result = Array.make_matrix rows_a cols_b 0.0 in
  if cols_a <> rows_b then invalid_arg "Unmatched dimensions";
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

let to_string a =
  let rows = Array.length a in
  let cols = Array.length a.(0) in
  let str_builder = ref "" in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      str_builder := !str_builder ^ string_of_float a.(i).(j) ^ " "
    done;
    str_builder := !str_builder ^ "\n"
  done;
  !str_builder
