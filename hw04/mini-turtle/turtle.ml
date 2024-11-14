
open Graphics

type color = Graphics.color
let black = Graphics.black
let white = Graphics.white
let red = Graphics.red
let green = Graphics.green
let blue = Graphics.blue
let set_color = Graphics.set_color

module A = struct
  type t = float
  let add = (+.)
  let pi_over_180 = atan 1. /. 45.
  let of_degrees d = d *. pi_over_180
  let cos = Stdlib.cos
  let sin = Stdlib.sin
end

let draw = ref true
let pen_down () = draw := true
let pen_up   () = draw := false

let angle = ref (A.of_degrees 0.)
let turn_left d = angle := A.add !angle (A.of_degrees (float d))
let turn_right d = turn_left (- d)

open Graphics
let tx = ref 400.
let ty = ref 400.
let () = open_graph " 800x800"; set_line_width 2;
  moveto (truncate !tx) (truncate !ty)

let forward d =
  tx := !tx +. float d *. A.cos !angle;
  ty := !ty +. float d *. A.sin !angle;
  if !draw then lineto (truncate !tx) (truncate !ty)
           else moveto (truncate !tx) (truncate !ty)

