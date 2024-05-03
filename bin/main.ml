open Final_project
open Bogue
module W = Widget
module L = Layout

let label_text = ref ""
let matrix_display = ref false
let matrix_operation = ref 0
let string1 = ref ""
let string1_done = ref false
let operation = ref Basicops.add
let string2 = ref ""
let output = ref ""

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
    ~action:(fun _ -> W.set_text label (add_text label "π"))

let comma =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) ","
    ~action:(fun _ -> W.set_text label (add_text label ","))

let left_s_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "["
    ~action:(fun _ -> W.set_text label (add_text label "["))

let right_s_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "]"
    ~action:(fun _ -> W.set_text label (add_text label "]"))

let left_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "("
    ~action:(fun _ -> W.set_text label (add_text label "("))

let right_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) ")"
    ~action:(fun _ -> W.set_text label (add_text label ")"))

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
      matrix_operation := 0;
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
        if !matrix_operation = 1 then
          output :=
            Matrix.to_calculator_string
              (Matrix.identity_matrix (int_of_string !string1)))
      else
        output :=
          string_of_float
            (!operation (float_of_string !string1) (float_of_string !string2));
      W.set_text label_output (add_text label_output !output))

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
      else (
        if !string1_done = false then string1 := !string1 ^ "1"
        else string2 := !string2 ^ "1";
        W.set_text label (add_text label "1")))

let two_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "2"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "2"
      else string2 := !string2 ^ "2";
      W.set_text label (add_text label "2"))

let three_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "3"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "3"
      else string2 := !string2 ^ "3";
      W.set_text label (add_text label "3"))

let four_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "4"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "4"
      else string2 := !string2 ^ "4";
      W.set_text label (add_text label "4"))

let five_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "5"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "5"
      else string2 := !string2 ^ "5";
      W.set_text label (add_text label "5"))

let six_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "6"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "6"
      else string2 := !string2 ^ "6";
      W.set_text label (add_text label "6"))

let seven_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "7"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "7"
      else string2 := !string2 ^ "7";
      W.set_text label (add_text label "7"))

let eight_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "8"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "8"
      else string2 := !string2 ^ "8";
      W.set_text label (add_text label "8"))

let nine_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "9"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "9"
      else string2 := !string2 ^ "9";
      W.set_text label (add_text label "9"))

let zero_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "0"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "0"
      else string2 := !string2 ^ "0";
      W.set_text label (add_text label "0"))

let dot_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "."
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "."
      else string2 := !string2 ^ ".";
      W.set_text label (add_text label "."))

let negative_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "-"
    ~action:(fun _ ->
      if !string1_done = false then string1 := !string1 ^ "-"
      else string2 := !string2 ^ "-";
      W.set_text label (add_text label "-"))

let stats_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Stats"

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
         4: Swap Rows\n\
         5: Swap Columns\n\
         6: Row Reduce\n\
         7: Rank")

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
      if !string1_done = false then
        string1 := String.sub !string1 0 (String.length !string1 - 1)
      else if !string2 = "" then (
        string1_done := false;
        operation := Basicops.add)
      else string2 := String.sub !string2 0 (String.length !string2 - 1);
      W.set_text label (delete_text label))

let trig_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Trig"

let poly_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Poly"

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
