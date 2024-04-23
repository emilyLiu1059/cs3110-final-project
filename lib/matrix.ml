let identity_matrix n =
  let result = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    result.(i).(i) <- 1.0
  done;
  result

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

let scalar_add matrix scalar =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let result = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- matrix.(i).(j) +. scalar
    done
  done;
  result

let scalar_subtract matrix scalar =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let result = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- matrix.(i).(j) -. scalar
    done
  done;
  result

let scalar_multiply matrix scalar =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let result = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- matrix.(i).(j) *. scalar
    done
  done;
  result

let scalar_divide matrix scalar =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let result = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      result.(i).(j) <- matrix.(i).(j) /. scalar
    done
  done;
  result

(* Helper function to remove a row and column from a matrix *)
let remove_row_col matrix row col =
  Array.init
    (Array.length matrix - 1)
    (fun i ->
      Array.init
        (Array.length matrix.(0) - 1)
        (fun j ->
          matrix.(if i < row then i else i + 1).(if j < col then j else j + 1)))

let rec determinant matrix =
  let n = Array.length matrix in
  if n = 1 then matrix.(0).(0)
  else if n = 2 then
    (matrix.(0).(0) *. matrix.(1).(1)) -. (matrix.(0).(1) *. matrix.(1).(0))
  else
    let det = ref 0.0 in
    for col = 0 to n - 1 do
      let sign = if col mod 2 = 0 then 1.0 else -1.0 in
      det :=
        !det
        +. sign
           *. matrix.(0).(col)
           *. determinant (remove_row_col matrix 0 col)
    done;
    !det

let swap_rows matrix row1 row2 =
  let result = Array.copy matrix in
  let temp = result.(row1) in
  result.(row1) <- result.(row2);
  result.(row2) <- temp;
  result

let swap_cols matrix col1 col2 =
  let rows = Array.length matrix in
  let result = Array.map Array.copy matrix in
  for i = 0 to rows - 1 do
    let temp = result.(i).(col1) in
    result.(i).(col1) <- result.(i).(col2);
    result.(i).(col2) <- temp
  done;
  result

let row_reduce matrix =
  let m = Array.length matrix in
  let n = Array.length matrix.(0) in
  let result = Array.map Array.copy matrix in
  let lead = ref 0 in
  let done_reduction = ref false in
  let rec loop r =
    if !done_reduction || n <= !lead then ()
    else if r = m then loop 0
    else
      let i = ref r in
      while (not !done_reduction) && result.(!i).(!lead) = 0.0 do
        i := !i + 1;
        if m = !i then (
          i := r;
          lead := !lead + 1;
          if n = !lead then done_reduction := true)
      done;
      if not !done_reduction then (
        let pivot_val = result.(!i).(!lead) in
        for j = 0 to n - 1 do
          result.(r).(j) <- result.(r).(j) /. pivot_val
        done;
        for i = 0 to m - 1 do
          if i <> r then
            let scale = result.(i).(!lead) in
            for j = 0 to n - 1 do
              result.(i).(j) <- result.(i).(j) -. (scale *. result.(r).(j))
            done
        done;
        lead := !lead + 1;
        loop (r + 1))
  in
  loop 0;
  result

let rank matrix =
  let m = Array.length matrix in
  let n = Array.length matrix.(0) in
  let reduced_matrix = row_reduce matrix in
  let rank = ref 0 in
  for i = 0 to min m n - 1 do
    if reduced_matrix.(i).(i) <> 0.0 then rank := !rank + 1
  done;
  !rank

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
