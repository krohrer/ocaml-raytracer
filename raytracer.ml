#use "topfind";;
#require "Graphics";;

module G = Graphics

type vec_t = { x : float; y : float; z : float }

let ( +| ) a b = { x = a.x +. b.x;
		   y = a.y +. b.y;
		   z = a.z +. b.z }

let ( -| ) a b = { x = a.x -. b.x; 
		   y = a.y -. b.y;
		   z = a.z -. b.z }

let ( *| ) a b = { x = a.x *. b.x; 
		   y = a.y *. b.y;
		   z = a.z *. b.z }

let ( /| ) a b = { x = a.x /. b.x; 
		   y = a.y /. b.y;
		   z = a.z /. b.z }

let ( *.| ) s a = { x = s *. a.x;
		    y = s *. a.y;
		    z = s *. a.z }

let vone  = { x = 1.; y = 1.; z = 1. }
let vzero = { x = 0.; y = 0.; z = 0. }

let vcross a b = { x = a.y *. b.z -. a.z *. b.y;
		   y = a.z *. b.x -. a.x *. b.z;
		   z = a.x *. b.y -. a.y *. b.x }

let vdot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z
let vlen_sq a = vdot a a
let vlen a = sqrt (vlen_sq a)
let vnormalize a = (1./.vlen a) *.| a
let vdist_sq a b = vlen_sq (a -| b)
let vdist a b = vlen (a -| b)

type ray_t = { origin : vec_t; dir : vec_t; invdir : vec_t }

let make_ray ?(origin=vzero) ~dir () =
  { origin;
    dir = vnormalize dir;
    invdir = vone /| dir }

type sphere_t = { center : vec_t; radius : float }

type scene_t =
  | Sphere of sphere_t

let ray_intersect_scene r = function
  | Sphere s -> 
    (* Solve distance(r.dir*t, center') = s.radius *)
    let o = r.origin -| s.center and
	d = r.dir in
    let a = vdot d d and
	b = 2. *. vdot o d and
	c = vdot o o -. s.radius *. s.radius in
    let d = b*. -. 4.*.a*.c in
    if d < 0. then
      None
    else
      let t = (~-.b -. sqrt d)/.(2.*.a) in
      Some t

let render ~w ~h ~set_pixel =
  let scene = Sphere {center={x=1.;y=0.;z=1.};radius=1.0} in
  for ix = 0 to w-1 do
    for iy = 0 to h-1 do
      let screen_pos = { x = float_of_int (2*ix-w) /. float_of_int h;
			 y = float_of_int (2*iy-h) /. float_of_int h;
			 z = 1.0 } in
      let ray = make_ray ~dir:screen_pos () in
      match ray_intersect_scene ray scene with
      | Some _ -> set_pixel ix iy {x=1.;y=0.;y=0.}
      | None -> set_pixel ix iy vzero
    done
  done

let _ =
  G.open_graph "";
  G.set_window_title "A simple raytracer in OCaml";
  let color_from_vec { x; y; z } =
    let r = int_of_float (255. *. x) and
	g = int_of_float (255. *. y) and
	b = int_of_float (255. *. z) in
    G.rgb (r land 255) (g land 255) (b land 255)
  in
  let set_pixel x y color_vec =
    G.set_color (color_from_vec color_vec);
    G.plot x y
  in
  let width = G.size_x () and height = G.size_y () in 
  G.set_color G.red;
  G.draw_rect 0 0 10 10;
  render ~w:10 ~h:10 ~set_pixel;
  ignore (G.wait_next_event [G.Button_down]);
  G.close_graph ()

