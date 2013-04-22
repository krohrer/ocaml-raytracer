(* A Magic interface that handles all the window system stuff. *)

type set_pixel_t = x:int -> y:int -> r:float -> g:float -> b:float -> unit
type renderer_t = w:int -> h:int -> set_pixel:set_pixel_t -> unit

val render_and_display : renderer_t -> unit
