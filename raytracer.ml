#use "topfind";;
#require "Graphics";;

module G = Graphics

type vec_t = { x : float; y : float; z : float }

let ( +| ) a b = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
let ( -| ) a b = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
let ( *| ) a b = { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z }
let ( /| ) a b = { x = a.x /. b.x; y = a.y /. b.y; z = a.z /. b.z }
let ( *.| ) s a = { x = s *. a.x; y = s *. a.y; z = s *. a.z }

let vec x y z = { x; y; z }

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
type plane_t = { normal : vec_t; anchor : vec_t }

let sphere center radius = { center; radius }

type object_t =
  | Sphere of sphere_t
  | Plane of plane_t

let ray_intersect_sphere r tmax s =
    (* Solve distance(r.dir*t, center') = s.radius *)
  let o = r.origin -| s.center and
      d = r.dir in
  let a = vdot d d and
      b = 2. *. vdot o d and
      c = vdot o o -. s.radius *. s.radius in
  let d = b*.b -. 4.*.a*.c in
  if d < 0. then
    tmax
  else
    let t = (~-.b -. sqrt d)/.(2.*.a) in
    min t tmax

let ray_intersect_plane r tmax p =
  let a = vdot (p.anchor -| r.origin) p.normal and
      b = vdot r.dir p.normal in
  if b = 0. then
    tmax
  else
    let t = a /. b in
    if t > 0. then
      min t tmax
    else
      tmax

let ray_intersect_obj r tmax = function
  | Sphere s -> ray_intersect_sphere r tmax s
  | Plane p -> ray_intersect_plane r tmax p

let ray_intersect_objs r tmax objs =
  List.fold_left (ray_intersect_obj r) tmax objs

let render ~w ~h ~set_pixel =
  let scene = [Sphere {center={x=1.;y=0.;z=10.};radius=1.0};
	       Plane {anchor=vec 0. 1. 0.; normal=vec 0. ~-.1. 0.}] in
  for ix = 0 to w-1 do
    for iy = 0 to h-1 do
      let screen_pos = { x = float_of_int (2*ix-w) /. float_of_int h;
			 y = float_of_int (h-2*iy) /. float_of_int h;
			 z = 1.0 } in
      let ray = make_ray ~dir:screen_pos () in
      let t = ray_intersect_objs ray infinity scene in
      set_pixel ix iy (vec t 0. 0.)
    done
  done

let _ =
  G.open_graph "";
  G.set_window_title "A simple raytracer in OCaml";
  G.resize_window 400 300;
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
  render ~w:width ~h:height ~set_pixel;
  ignore (G.wait_next_event [G.Key_pressed]);
  G.close_graph ()

