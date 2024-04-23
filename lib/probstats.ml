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

let skewness lst =
  let m = mean lst in
  let n = float_of_int (List.length lst) in
  let third_mom =
    List.fold_left (fun acc x -> acc +. ((x -. m) ** 3.)) 0. lst
  in
  n /. ((n -. 1.) *. (n -. 2.)) *. (third_mom /. (sqrt (variance lst) ** 3.))

let z_score x lst =
  let m = mean lst in
  let s = sd lst in
  (x -. m) /. s

let iqr lst =
  let q1 = quantile 0.25 lst in
  let q3 = quantile 0.75 lst in
  q3 -. q1

let coefficient_of_variation lst =
  let s = sd lst in
  let m = mean lst in
  s /. m *. 100.

let correlation lst1 lst2 =
  if List.length lst1 <> List.length lst2 then
    failwith "Lists must be of equal length"
  else
    let mean1 = mean lst1 in
    let mean2 = mean lst2 in
    let n = float_of_int (List.length lst1) in
    let sum_xy =
      List.fold_left2
        (fun acc x y -> acc +. ((x -. mean1) *. (y -. mean2)))
        0. lst1 lst2
    in
    let sum_xx =
      List.fold_left (fun acc x -> acc +. ((x -. mean1) ** 2.)) 0. lst1
    in
    let sum_yy =
      List.fold_left (fun acc y -> acc +. ((y -. mean2) ** 2.)) 0. lst2
    in
    let std_dev_x = sqrt (sum_xx /. (n -. 1.)) in
    let std_dev_y = sqrt (sum_yy /. (n -. 1.)) in
    sum_xy /. (std_dev_x *. std_dev_y)

let cdf x lst =
  let sorted = List.sort compare lst in
  let rec count_leq count elements =
    match elements with
    | [] -> count
    | h :: t when h <= x -> count_leq (count + 1) t
    | _ -> count
  in
  float_of_int (count_leq 0 sorted) /. float_of_int (List.length lst)

let linear_regression xs ys =
  let mean_x = mean xs in
  let mean_y = mean ys in
  let ss_xy =
    List.fold_left2
      (fun acc x y -> acc +. ((x -. mean_x) *. (y -. mean_y)))
      0. xs ys
  in
  let ss_xx =
    List.fold_left (fun acc x -> acc +. ((x -. mean_x) ** 2.)) 0. xs
  in
  let slope = ss_xy /. ss_xx in
  let intercept = mean_y -. (slope *. mean_x) in
  (slope, intercept)
