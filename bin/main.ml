open Bogue
module W = Widget
module L = Layout

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

let label = W.label ""

let add_text l str =
  let current_text = W.get_text l in
  current_text ^ str

let pi =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "π"

let comma =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) ","

let left_s_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "["

let right_s_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "]"

let left_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "("

let right_bracket =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) ")"

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
    ~action:(fun _ -> W.set_text label "")

let add_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "+"
    ~action:(fun _ -> W.set_text label (add_text label "+"))

let subtract_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "-"
    ~action:(fun _ -> W.set_text label (add_text label "-"))

let multiply_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "x"
    ~action:(fun _ -> W.set_text label (add_text label "x"))

let divide_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "÷"
    ~action:(fun _ -> W.set_text label (add_text label "÷"))

let enter_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Enter"

let one_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "1"
    ~action:(fun _ -> W.set_text label (add_text label "1"))

let two_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "2"
    ~action:(fun _ -> W.set_text label (add_text label "2"))

let three_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "3"
    ~action:(fun _ -> W.set_text label (add_text label "3"))

let four_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "4"
    ~action:(fun _ -> W.set_text label (add_text label "4"))

let five_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "5"
    ~action:(fun _ -> W.set_text label (add_text label "5"))

let six_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "6"
    ~action:(fun _ -> W.set_text label (add_text label "6"))

let seven_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "7"
    ~action:(fun _ -> W.set_text label (add_text label "7"))

let eight_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "8"
    ~action:(fun _ -> W.set_text label (add_text label "8"))

let nine_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "9"
    ~action:(fun _ -> W.set_text label (add_text label "9"))

let zero_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "0"
    ~action:(fun _ -> W.set_text label (add_text label "0"))

let dot_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "."
    ~action:(fun _ -> W.set_text label (add_text label "."))

let negative_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "-"
    ~action:(fun _ -> W.set_text label (add_text label "-"))

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

let quit_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Quit"

let delete_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Delete"

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
  L.resident ~x:30 ~y:30 ~w:200 ~h:45
    ~background:
      (L.style_bg
         (Style.of_bg (Style.color_bg Draw.(opaque @@ find_color "white"))))
    ~draggable:false ~keyboard_focus:true label

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
      ]
  in
  L.set_background layout (Some (L.color_bg Draw.(opaque @@ find_color "grey")));
  let board = Bogue.of_layout layout in
  Bogue.run board

let () = example4 ()
