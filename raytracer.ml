(*
#use "topfind";;
#require "Graphics";;
*)

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

let make_sphere x y z ~r = { center=vec x y z; radius=r }
let make_plane x y z d =
  let normal = vnormalize (vec x y z) in
  let anchor = ~-. d *.| normal in
  { normal; anchor }

type material_t =
  | Mirror
  | Checkerboard of [`XY | `YZ | `XZ]
  | Color of vec_t

type shape_t =
  | Sphere of sphere_t
  | Plane of plane_t

type object_t = material_t * shape_t

type intersection_t = (object_t * float) option

let ray_intersects_sphere r s =
    (* Solve distance(r.dir*t, center') = s.radius *)
  let o = r.origin -| s.center and
      d = r.dir in
  let a = vdot d d and
      b = 2. *. vdot o d and
      c = vdot o o -. s.radius *. s.radius in
  let d = b*.b -. 4.*.a*.c in
  if d < 0. then
    None
  else
    let t = (~-.b -. sqrt d)/.(2.*.a) in
    Some t

let ray_intersects_plane r p =
  let a = vdot (p.anchor -| r.origin) p.normal and
      b = vdot r.dir p.normal in
  if b = 0. then
    None
  else
    let t = a /. b in
    if t > 0. then
      Some t
    else
      None

let ray_intersect_object r (m,shape) =
  let topt = 
    match shape with 
    | Sphere s -> ray_intersects_sphere s
    | Plane p -> ray_intersects_plane p
  in
  match (
    match shape with 
    |
  | mat, Sphere s -> ray_intersects_object

let intersection_closest i1 i2 =
  match i1, i2 with
  | None, None -> None
  | None, i
  | i, None -> i
  | Some (_,t1), Some (_,t2) -> if t1 < t2 then i1 else i2

type scene_t = {
  objects : object_t list;
}

let scene =
  let color_red		= Color (vec 1. 0. 0.) and
      color_green	= Color (vec 0. 1. 0.) and
      color_blue	= Color (vec 0. 0. 1.) and 
      color_white	= Color vone and
      checkerboard	= Checkerboard `XZ in
  let sphere x y z r	= Sphere (make_sphere x y z ~r) and
      plane nx ny nz d	= Plane (make_plane nx ny nz d) in
  { objects = [ color_red	, sphere 10. 0. 10. 3.;
		color_green	, sphere 10. 5. 20. 7.;
		color_blue	, sphere 1. 0. 5. 1.;
		color_white	, sphere 20. 3. 30. 10.;
		checkerboard	, plane 0. 1. 0. 1. ]
  }

let scene_cast_ray { objects; _ } =
  
  let rec find_closest_intersection closest = function
    | [] -> closest
    | (mat,Sphere s) as o :: rest -> (
      match ray_intersects_sphere s with
      | None -> find_closes
    )
    | (mat,Plane p) as o :: rest -> (
    )

  
let render ~w ~h ~set_pixel =
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

