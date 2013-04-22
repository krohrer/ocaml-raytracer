module G = Graphics

type set_pixel_t = x:int -> y:int -> r:float -> g:float -> b:float -> unit
type renderer_t = w:int -> h:int -> set_pixel:set_pixel_t -> unit

let render_and_display renderer =
  G.open_graph "";
  G.set_window_title "A simple raytracer in OCaml";
  ignore (G.wait_next_event [G.Key_pressed]);
  let width = G.size_x () and height = G.size_y () in
  let set_pixel ~x ~y ~r ~g ~b =
    let r = int_of_float (255. *. r) and
	g = int_of_float (255. *. g) and
	b = int_of_float (255. *. b) in
    let rgb = G.rgb (min r 255) (min g 255) (min b 255) in
    G.set_color rgb;
    G.plot x y
  in
  renderer ~w:width ~h:height ~set_pixel;
  ignore (G.wait_next_event [G.Key_pressed]);
  G.close_graph ()
