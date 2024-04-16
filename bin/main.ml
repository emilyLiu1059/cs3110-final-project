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

let add_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "+"

let subtract_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "-"

let multiply_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "x"

let divide_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "÷"

let enter_button =
  W.button ~kind:Trigger ~fg:(Draw.opaque Draw.black)
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~border_radius:10 ~border_color:(Draw.opaque Draw.grey) "Enter"

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

let example4 () =
  let box = W.box ~w:500 ~h:600 ~style:round_blue_box () in
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
      ]
  in
  L.set_background layout (Some (L.color_bg Draw.(opaque @@ find_color "grey")));
  let board = Bogue.of_layout layout in
  Bogue.run board

let () = example4 ()
