
(* The turtle logo *)

val pen_up: unit -> unit
val pen_down: unit -> unit

val forward: int -> unit
  (** distance in pixels *)

val turn_left: int -> unit
val turn_right: int -> unit
  (** angles in degree *)

type color
val black: color
val white: color
val red  : color
val green: color
val blue : color

val set_color: color -> unit
