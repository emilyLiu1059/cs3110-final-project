let mean lst =
  let sum = List.fold_left ( +. ) 0. lst in
  sum /. float_of_int (List.length lst)

let median lst =
  let sorted = List.sort compare lst in
  let len = List.length sorted in
  if len mod 2 = 0 then
    (List.nth sorted ((len / 2) - 1) +. List.nth sorted (len / 2)) /. 2.0
  else List.nth sorted (len / 2)

let variance lst =
  let m = mean lst in
  let squared_diffs = List.map (fun x -> (x -. m) ** 2.) lst in
  mean squared_diffs

let sd lst = sqrt (variance lst)

let quantile q lst =
  let sorted = List.sort compare lst in
  let index = int_of_float (q *. float_of_int (List.length sorted - 1)) in
  List.nth sorted index

let range lst =
  let sorted = List.sort compare lst in
  List.hd (List.rev sorted) -. List.hd sorted

let mode lst =
  let tbl = Hashtbl.create (List.length lst) in
  List.iter
    (fun x ->
      Hashtbl.replace tbl x ((try Hashtbl.find tbl x with Not_found -> 0) + 1))
    lst;
  let mode, _ =
    Hashtbl.fold
      (fun key value (mkey, mval) ->
        if value > mval then (key, value) else (mkey, mval))
      tbl
      (List.hd lst, 0)
  in
  mode

let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
let permutation n r = factorial n / factorial (n - r)
let combination n r = permutation n r / factorial r
