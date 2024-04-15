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

let example4 () =
  let left_arrow =
    L.resident ~x:400 ~y:350 ~w:30 ~h:25 (W.button ~kind:Trigger "<")
  in
  let right_arrow =
    L.resident ~x:460 ~y:350 ~w:30 ~h:25 (W.button ~kind:Trigger ">")
  in
  let up_arrow =
    L.resident ~x:430 ~y:325 ~w:30 ~h:25 (W.button ~kind:Trigger "^")
  in
  let down_arrow =
    L.resident ~x:430 ~y:375 ~w:30 ~h:25 (W.button ~kind:Trigger "v")
  in
  let box = W.box ~w:500 ~h:600 ~style:round_blue_box () in
  let screen = W.box ~w:500 ~h:300 ~style:round_grey_box () in
  let layout =
    L.superpose ~name:"Calculator"
      [
        L.tower_of_w [ screen; box ];
        left_arrow;
        right_arrow;
        up_arrow;
        down_arrow;
      ]
  in
  L.set_background layout (Some (L.color_bg Draw.(opaque @@ find_color "grey")));
  let board = Bogue.of_layout layout in
  Bogue.run board

let () = example4 ()
