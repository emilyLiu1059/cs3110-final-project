open Final_project
open Bogue
module W = Widget
module L = Layout

let label_text = ref ""
let matrix_display = ref false
let stats_display = ref false
let trig_display = ref false
let poly_display = ref false
let matrix_operation = ref 0
let stats_operation = ref 0
let trig_operation = ref 0
let poly_operation = ref 0
let string1 = ref ""
let string1_done = ref false
let operation = ref Basicops.add
let string2 = ref ""
let output = ref ""

let string_to_float_list s =
  let clean_str = String.trim s in
  let stripped = String.sub clean_str 1 (String.length clean_str - 2) in
  let items = String.split_on_char ',' stripped in
  List.map String.trim items
  |> List.filter_map (fun item ->
         try Some (float_of_string item)
         with Failure _ -> None (* Optionally log the error here *))

let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let round_blue_box =
  let open Style in
  let border = mk_border ~radius:25 thick_grey_line in
  create ~border
    ~background:(color_bg Draw.(opaque @@ find_color "lightblue"))
    ()

let round_grey_box =
  let open Style in
  let border = mk_border ~radius:25 thick_grey_line in
  create ~border ~background:(color_bg Draw.(opaque @@ find_color "white")) ()

let label = W.label ~size:25 ~align:Min ""
let label_output = W.label ~size:25 ~align:Max ""
let text_display = W.text_display ""

let add_text l str =
  let current_text = W.get_text l in
  current_text ^ str

let delete_text l =
  let current_text = W.get_text l in
  String.sub current_text 0 (String.length current_text - 1)

let pi =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "π"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "3.141592653"
      else string2 := !string2 ^ "3.141592653";
      W.set_text label (add_text label "3.141592653"))

let comma =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) ","
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ ","
      else string2 := !string2 ^ ",";
      W.set_text label (add_text label ","))

let left_s_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "["
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "["
      else string2 := !string2 ^ "[";
      W.set_text label (add_text label "["))

let right_s_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "]"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "]"
      else string2 := !string2 ^ "]";
      W.set_text label (add_text label "]"))

let left_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "("
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "()"
      else string2 := !string2 ^ "()";
      W.set_text label (add_text label "("))

let right_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) ")"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ ")"
      else string2 := !string2 ^ ")";
      W.set_text label (add_text label ")"))

let x =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "x"

let left_arrow =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "<"

let right_arrow =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) ">"

let up_arrow =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "^"

let down_arrow =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "v"

let clear_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Clear"
    ~action:(fun _ ->
      label_text := "";
      matrix_display := false;
      stats_display := false;
      poly_display := false;
      matrix_operation := 0;
      stats_operation := 0;
      poly_operation := 0;
      string1 := "";
      string1_done := false;
      operation := Basicops.add;
      string2 := "";
      output := "";
      W.set_text label "";
      W.set_text label_output "";
      W.set_text text_display "")

let add_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "+"
    ~action:(fun _ ->
      operation := Basicops.add;
      string1_done := true;
      W.set_text label (add_text label "+"))

let subtract_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "-"
    ~action:(fun _ ->
      operation := Basicops.subtract;
      string1_done := true;
      W.set_text label (add_text label "-"))

let multiply_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "x"
    ~action:(fun _ ->
      operation := Basicops.multiply;
      string1_done := true;
      W.set_text label (add_text label "x"))

let divide_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "÷"
    ~action:(fun _ ->
      operation := Basicops.divide;
      string1_done := true;
      W.set_text label (add_text label "÷"))

let enter_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Enter"
    ~action:(fun _ ->
      if !matrix_operation > 0 then (
        (* Existing matrix operation logic *)
        (output :=
           match !matrix_operation with
           | 1 ->
               Matrix.to_calculator_string
                 (Matrix.identity_matrix (int_of_string !string1))
           | 2 ->
               Matrix.to_calculator_string
                 (Matrix.transpose (Matrix.from_calculator_string !string1))
           | 3 ->
               string_of_float
                 (Matrix.determinant (Matrix.from_calculator_string !string1))
           | 4 ->
               Matrix.to_calculator_string
                 (Matrix.row_reduce (Matrix.from_calculator_string !string1))
           | 5 ->
               string_of_int
                 (Matrix.rank (Matrix.from_calculator_string !string1))
           | 6 ->
               Matrix.to_calculator_string
                 (Matrix.add
                    (Matrix.from_calculator_string !string1)
                    (Matrix.from_calculator_string !string2))
           | 7 ->
               Matrix.to_calculator_string
                 (Matrix.subtract
                    (Matrix.from_calculator_string !string1)
                    (Matrix.from_calculator_string !string2))
           | 8 ->
               Matrix.to_calculator_string
                 (Matrix.multiply
                    (Matrix.from_calculator_string !string1)
                    (Matrix.from_calculator_string !string2))
           | 9 ->
               Matrix.to_calculator_string
                 (Matrix.scalar_divide
                    (Matrix.from_calculator_string !string1)
                    (float_of_string !string2))
           | _ -> failwith "Unsupported matrix operation");
        matrix_display := false;
        W.set_text label_output (add_text label_output !output))
      else if !poly_operation > 0 then (
        (* Existing poly operation logic *)
        (output :=
           match !poly_operation with
           | 1 -> string_of_float (Poly.rad_to_deg (float_of_string !string1))
           | 2 -> string_of_float (Poly.deg_to_rad (float_of_string !string1))
           | 3 ->
               string_of_float
                 (Poly.eval (float_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 4 ->
               Poly.string_of_float_list
                 (Poly.deri_once (Poly.float_list_of_string !string1))
           | 5 ->
               Poly.string_of_float_list
                 (Poly.inte_once (float_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 6 ->
               Poly.string_of_float_list
                 (Poly.add
                    (Poly.float_list_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 7 ->
               Poly.string_of_float_list
                 (Poly.subtract
                    (Poly.float_list_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 8 ->
               Poly.string_of_float_list
                 (Poly.multiply
                    (Poly.float_list_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 9 ->
               Poly.string_of_tuple_float_list
                 (Poly.divide_polynomials
                    (Poly.float_list_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 10 ->
               Poly.string_of_float_list
                 (Poly.gcd_polynomial
                    (Poly.float_list_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 11 ->
               Poly.string_of_float_list
                 (Poly.compose_polynomials
                    (Poly.float_list_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | 12 ->
               Poly.string_of_float_list
                 (Poly.eval_at_points
                    (Poly.float_list_of_string !string1)
                    (Poly.float_list_of_string !string2))
           | _ -> failwith "Unsupported matrix operation");

        matrix_display := false;
        W.set_text label_output (add_text label_output !output))
      else if !trig_operation > 0 then (
        (match !trig_operation with
        | 1 -> output := string_of_float (Trig.sin (float_of_string !string1))
        | 2 -> output := string_of_float (Trig.cos (float_of_string !string1))
        | 3 -> output := string_of_float (Trig.tan (float_of_string !string1))
        | 4 -> output := string_of_float (Trig.csc (float_of_string !string1))
        | 5 -> output := string_of_float (Trig.sec (float_of_string !string1))
        | 6 -> output := string_of_float (Trig.cot (float_of_string !string1))
        | 7 ->
            output := string_of_float (Trig.arcsin (float_of_string !string1))
        | 8 ->
            output := string_of_float (Trig.arccos (float_of_string !string1))
        | 9 ->
            output := string_of_float (Trig.arctan (float_of_string !string1))
        | _ -> failwith "Unsupported trig operation");
        trig_display := false;
        W.set_text label_output (add_text label_output !output))
      else if !stats_operation > 0 then (
        (* Use the improved string_to_float_list for stats operations *)
        let data = string_to_float_list !string1 in
        (output :=
           match !stats_operation with
           | 1 -> string_of_float (Probstats.mean data)
           | 2 -> string_of_float (Probstats.median data)
           | 3 -> string_of_float (Probstats.variance data)
           | 4 -> string_of_float (Probstats.sd data)
           | 5 -> string_of_float (Probstats.range data)
           | 6 -> string_of_float (Probstats.mode data)
           | 7 -> string_of_float (Probstats.quantile 0.5 data)
           | 8 -> string_of_float (Probstats.skewness data)
           | _ -> failwith "Unsupported stats operation");
        stats_display := false;
        W.set_text label_output (add_text label_output !output))
      else (
        output :=
          string_of_float
            (!operation (float_of_string !string1) (float_of_string !string2));
        W.set_text label_output (add_text label_output !output)))

let one_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "1"
    ~action:(fun _ ->
      if !matrix_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Identity ");
        matrix_operation := 1;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Mean ");
        stats_operation := 1;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Sin ");
        trig_operation := 1;
        trig_display := false)
      else if !poly_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Radian to Degree ");
        poly_operation := 1;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "1"
        else string2 := !string2 ^ "1";
        W.set_text label (add_text label "1")))

let two_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "2"
    ~action:(fun _ ->
      if !matrix_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Transpose ");
        matrix_operation := 2;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Median ");
        stats_operation := 2;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Cos ");
        trig_operation := 2;
        trig_display := false)
      else if !poly_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Degree to Radian ");
        poly_operation := 2;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "2"
        else string2 := !string2 ^ "2";
        W.set_text label (add_text label "2")))

let three_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "3"
    ~action:(fun _ ->
      if !matrix_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Determinant ");
        matrix_operation := 3;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Variance ");
        stats_operation := 3;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Tan ");
        trig_operation := 3;
        trig_display := false)
      else if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Eval ");
        poly_operation := 3;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "3"
        else string2 := !string2 ^ "3";
        W.set_text label (add_text label "3")))

let four_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "4"
    ~action:(fun _ ->
      if !matrix_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Row Reduce ");
        matrix_operation := 4;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Standard Deviation ");
        stats_operation := 4;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Csc ");
        trig_operation := 4;
        trig_display := false)
      else if !poly_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Derivative ");
        poly_operation := 4;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "4"
        else string2 := !string2 ^ "4";
        W.set_text label (add_text label "4")))

let five_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "5"
    ~action:(fun _ ->
      if !matrix_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Rank ");
        matrix_operation := 5;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Range ");
        stats_operation := 5;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Sec ");
        trig_operation := 5;
        trig_display := false)
      else if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Integrate ");
        poly_operation := 5;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "5"
        else string2 := !string2 ^ "5";
        W.set_text label (add_text label "5")))

let six_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "6"
    ~action:(fun _ ->
      if !matrix_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Add ");
        matrix_operation := 6;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Mode ");
        stats_operation := 6;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Cot ");
        trig_operation := 6;
        trig_display := false)
      else if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Add ");
        poly_operation := 6;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "6"
        else string2 := !string2 ^ "6";
        W.set_text label (add_text label "6")))

let seven_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "7"
    ~action:(fun _ ->
      if !matrix_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Subtract ");
        matrix_operation := 7;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Quantile ");
        stats_operation := 7;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " ArcSin ");
        trig_operation := 7;
        trig_display := false)
      else if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Subtract ");
        poly_operation := 7;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "7"
        else string2 := !string2 ^ "7";
        W.set_text label (add_text label "7")))

let eight_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "8"
    ~action:(fun _ ->
      if !matrix_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Multiply ");
        matrix_operation := 8;
        matrix_display := false)
      else if !stats_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Skewness ");
        stats_operation := 8;
        stats_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " ArcCos ");
        trig_operation := 8;
        trig_display := false)
      else if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Multiply ");
        poly_operation := 8;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "8"
        else string2 := !string2 ^ "8";
        W.set_text label (add_text label "8")))

let nine_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "9"
    ~action:(fun _ ->
      if !matrix_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Divide ");
        matrix_operation := 9;
        matrix_display := false)
      else if !trig_display then (
        W.set_text text_display "";
        W.set_text label (!label_text ^ " ArcTan ");
        trig_operation := 9;
        trig_display := false)
      else if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Divide ");
        poly_operation := 9;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "9"
        else string2 := !string2 ^ "9";
        W.set_text label (add_text label "9")))

let zero_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "0"
    ~action:(fun _ ->
      if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Greatest Common Divisor ");
        poly_operation := 10;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "0"
        else string2 := !string2 ^ "0";
        W.set_text label (add_text label "0")))

let dot_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "."
    ~action:(fun _ ->
      if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Compose ");
        poly_operation := 11;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "."
        else string2 := !string2 ^ ".";
        W.set_text label (add_text label ".")))

let negative_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "-"
    ~action:(fun _ ->
      if !poly_display then (
        string1_done := true;
        W.set_text text_display "";
        W.set_text label (!label_text ^ " Eval at points ");
        poly_operation := 12;
        poly_display := false)
      else (
        if !string1_done = false then string1 := !string1 ^ "-"
        else string2 := !string2 ^ "-";
        W.set_text label (add_text label "-")))

let stats_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Stats"
    ~action:(fun _ ->
      label_text := W.get_text label;
      stats_display := true;
      W.set_text label "";
      W.set_text label_output "";
      W.set_text text_display
        "1: Mean\n\
         2: Median\n\
         3: Variance\n\
         4: Standard Deviation\n\
         5: Range\n\
         6: Mode\n\
         7: Quantile\n\
         8: Skewness")

let e_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "e"

let matrix_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Matrix"
    ~action:(fun _ ->
      label_text := W.get_text label;
      matrix_display := true;
      W.set_text label "";
      W.set_text label_output "";
      W.set_text text_display
        "1: Identity Matrix\n\
         2: Transpose\n\
         3: Determinant\n\
         4: Row Reduce\n\
         5: Rank\n\
         6: Add\n\
         7: Subtract\n\
         8: Multiply\n\
         9: Divide")

let quit_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Quit"
    ~action:(fun _ -> exit 0)

let delete_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Delete"
    ~action:(fun _ ->
      if String.length (W.get_text label) > 0 then
        if !matrix_display || !stats_display || !poly_display then (
          (* Resetting the specific displays and operations when in matrix or
             stats mode *)
          matrix_display := false;
          stats_display := false;
          poly_display := false;
          matrix_operation := 0;
          stats_operation := 0;
          poly_operation := 0;
          string1 := "";
          string1_done := false;
          operation := Basicops.add;
          string2 := "";
          output := "";
          W.set_text label "";
          W.set_text label_output "";
          W.set_text text_display "")
        else (
          (* Normal delete operation when not in specific modes *)
          if !string1_done = false then
            string1 := String.sub !string1 0 (String.length !string1 - 1)
          else if !string2 = "" then (
            string1_done := false;
            operation := Basicops.add)
          else string2 := String.sub !string2 0 (String.length !string2 - 1);
          W.set_text label (delete_text label)))

let trig_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Trig"
    ~action:(fun _ ->
      label_text := W.get_text label;
      trig_display := true;
      W.set_text label "";
      W.set_text label_output "";
      W.set_text text_display
        "1: Sin\n\
         2: Cos\n\
         3: Tan\n\
         4: Csc\n\
         5: Sec\n\
         6: Cot\n\
         7: ArcSin\n\
         8: ArcCos\n\
         9: ArcTan")

let poly_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Poly"
    ~action:(fun _ ->
      label_text := W.get_text label;
      poly_display := true;
      W.set_text label "";
      W.set_text label_output "";
      W.set_text text_display
        "1: rad_to_deg\n\
         2: deg_to_rad\n\
         3: eval\n\
         4: derivative\n\
         5: integrate\n\
         6: add\n\
         7: subtract\n\
         8: multiply\n\
         9: divide\n\
         0: gcd\n\
         ,: compose\n\
         -: eval_at_points\n\
        \         ")

let pi_layout =
  L.resident ~x:30 ~y:610 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true pi

let comma_layout =
  L.resident ~x:80 ~y:610 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true comma

let left_s_bracket_layout =
  L.resident ~x:30 ~y:665 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true left_s_bracket

let right_s_bracket_layout =
  L.resident ~x:80 ~y:665 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true right_s_bracket

let right_bracket_layout =
  L.resident ~x:80 ~y:720 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true right_bracket

let left_bracket_layout =
  L.resident ~x:30 ~y:720 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true left_bracket

let x_layout =
  L.resident ~x:30 ~y:775 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true x

let left_arrow_layout =
  L.resident ~x:380 ~y:390 ~w:30 ~h:25
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true left_arrow

let right_arrow_layout =
  L.resident ~x:450 ~y:390 ~w:30 ~h:25
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true right_arrow

let up_arrow_layout =
  L.resident ~x:415 ~y:365 ~w:30 ~h:25
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true up_arrow

let down_arrow_layout =
  L.resident ~x:415 ~y:415 ~w:30 ~h:25
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true down_arrow

let clear_button_layout =
  L.resident ~x:380 ~y:470 ~w:100 ~h:35
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true clear_button

let add_button_layout =
  L.resident ~x:405 ~y:530 ~w:55 ~h:50
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true add_button

let subtract_button_layout =
  L.resident ~x:405 ~y:605 ~w:55 ~h:50
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true subtract_button

let multiply_button_layout =
  L.resident ~x:405 ~y:680 ~w:55 ~h:50
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true multiply_button

let divide_button_layout =
  L.resident ~x:405 ~y:755 ~w:55 ~h:50
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true divide_button

let enter_button_layout =
  L.resident ~x:380 ~y:830 ~w:100 ~h:35
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true enter_button

let one_button_layout =
  L.resident ~x:140 ~y:530 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true one_button

let two_button_layout =
  L.resident ~x:215 ~y:530 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true two_button

let three_button_layout =
  L.resident ~x:290 ~y:530 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true three_button

let four_button_layout =
  L.resident ~x:140 ~y:610 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true four_button

let five_button_layout =
  L.resident ~x:215 ~y:610 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true five_button

let six_button_layout =
  L.resident ~x:290 ~y:610 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true six_button

let seven_button_layout =
  L.resident ~x:140 ~y:690 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true seven_button

let eight_button_layout =
  L.resident ~x:215 ~y:690 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true eight_button

let nine_button_layout =
  L.resident ~x:290 ~y:690 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true nine_button

let zero_button_layout =
  L.resident ~x:140 ~y:770 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true zero_button

let dot_button_layout =
  L.resident ~x:215 ~y:770 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true dot_button

let negative_button_layout =
  L.resident ~x:290 ~y:770 ~w:65 ~h:70
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true negative_button

let e_button_layout =
  L.resident ~x:80 ~y:775 ~w:40 ~h:40
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true e_button

let matrix_button_layout =
  L.resident ~x:50 ~y:380 ~w:85 ~h:45
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true matrix_button

let quit_button_layout =
  L.resident ~x:150 ~y:380 ~w:85 ~h:45
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true quit_button

let delete_button_layout =
  L.resident ~x:250 ~y:380 ~w:85 ~h:45
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true delete_button

let trig_button_layout =
  L.resident ~x:50 ~y:440 ~w:85 ~h:45
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true trig_button

let poly_button_layout =
  L.resident ~x:150 ~y:440 ~w:85 ~h:45
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true poly_button

let stats_button_layout =
  L.resident ~x:250 ~y:440 ~w:85 ~h:45
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "lightgrey"))))
    ~draggable:false ~keyboard_focus:true stats_button

let label_layout =
  L.resident ~x:30 ~y:30 ~w:460 ~h:115
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "white"))))
    ~draggable:false ~keyboard_focus:true label

let label_output_layout =
  L.resident ~x:30 ~y:175 ~w:460 ~h:115
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "white"))))
    ~draggable:false ~keyboard_focus:true label_output

let text_display_layout =
  L.resident ~x:30 ~y:30 ~w:460 ~h:260 ~draggable:false ~keyboard_focus:true
    text_display

let example4 () =
  let box = W.box ~w:500 ~h:560 ~style:round_blue_box () in
  let screen = W.box ~w:500 ~h:300 ~style:round_grey_box () in
  let layout =
    L.superpose ~name:"Calculator"
      [
        L.tower_of_w [ screen; box ];
        left_arrow_layout;
        right_arrow_layout;
        up_arrow_layout;
        down_arrow_layout;
        clear_button_layout;
        add_button_layout;
        subtract_button_layout;
        multiply_button_layout;
        divide_button_layout;
        enter_button_layout;
        one_button_layout;
        two_button_layout;
        three_button_layout;
        four_button_layout;
        five_button_layout;
        six_button_layout;
        seven_button_layout;
        eight_button_layout;
        nine_button_layout;
        zero_button_layout;
        dot_button_layout;
        negative_button_layout;
        stats_button_layout;
        pi_layout;
        comma_layout;
        left_s_bracket_layout;
        right_s_bracket_layout;
        left_bracket_layout;
        right_bracket_layout;
        x_layout;
        e_button_layout;
        matrix_button_layout;
        quit_button_layout;
        delete_button_layout;
        trig_button_layout;
        poly_button_layout;
        label_layout;
        label_output_layout;
        text_display_layout;
      ]
  in
  L.set_background layout (Some (L.color_bg Draw.(opaque @@ find_color "grey")));
  let board = Bogue.of_layout layout in
  Bogue.run board

let () = example4 ()
