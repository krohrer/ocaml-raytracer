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

let vdot a b = a.x*.b.x +. a.y*.b.y +. a.z*.b.z
let vlen_sq a = vdot a a
let vlen a = sqrt (vlen_sq a)
let vnormalize a = (1./.vlen a) *.| a
let vdist_sq a b = vlen_sq (a -| b)
let vdist a b = vlen (a -| b)

let vproject a b = ((vdot a b) /. (vdot b b)) *.| b
let vreflect a n = a -| 2.*.|(vproject a n)

type ray_t = { origin : vec_t; dir : vec_t }

let make_ray ?(origin=vzero) ~dir () =
  { origin;
    dir = vnormalize dir }

type sphere_t = { center : vec_t; radius : float }

let make_sphere x y z ~r = { center=vec x y z; radius=r }

type material_t =
  | Mirror
  | Diffuse of vec_t
  | ConstColor of vec_t

type object_t = material_t * sphere_t
type intersection_t = (object_t * float) option

let ray_intersect_object r ((_, s) as obj) =
    (* Solve distance(r.dir*t, center') = s.radius *)
  let o = r.origin -| s.center and
      d = r.dir in
  let a = vdot d d and
      b = 2. *. vdot o d and
      c = vdot o o -. s.radius*.s.radius in
  let d = b*.b -. 4.*.a*.c in
  if d < 0. then
    None
  else
    if a < 0. then 
      let t = (~-.b +. sqrt d)/.(2.*.a) in
      if t < 0.0001 then
	None
      else
	Some (obj, t)
    else
      let t = (~-.b -. sqrt d)/.(2.*.a) in
      if t < 0.0001 then
	None
      else
	Some (obj, t)

let intersection_min i1 i2 =
  match i1, i2 with
  | None, None -> None
  | i, None
  | None, i -> i
  | Some (_,t1), Some (_,t2) when t1 < t2 -> i1
  | _ -> i2

let ray_find_min_intersection_step r imin obj =
  let i = ray_intersect_object r obj in
  intersection_min i imin

type light_t = {
  color : vec_t;
  position : vec_t
}

let make_light x y z r g b = { position = vec x y z; color = vec r g b }

type scene_t = {
  ambient_color : vec_t;
  lights : light_t list;
  objects : object_t list;
}

let scene =
  let diffuse r g b = Diffuse (vec r g b) and
      color r g b = ConstColor (vec r g b) and
      diffuse_white = Diffuse vone in
  let sphere x y z r	= make_sphere x y z ~r in
  { ambient_color = vec 0.2 0.2 0.2;
    lights = [ make_light 0. 100. ~-.10.       	1. 1. 1.;
	       make_light ~-.10. 10. 2.		0.5 0.5 0.;
	       make_light 0. 8. 8.		0. 0. 1.];
    objects = [ diffuse_white		, sphere  4. 4. 2. 1.;
                diffuse_white		, sphere  2. 4. 2. 1.;
                diffuse_white		, sphere  4. 2. 2. 1.;
                diffuse_white		, sphere  2. 2. 2. 1.;
                diffuse_white        	, sphere  4. 4. 4. 1.;
                diffuse_white        	, sphere  2. 4. 4. 1.;
                diffuse_white   	, sphere  4. 2. 4. 1.;
                diffuse_white	        , sphere  2. 2. 4. 1.;
                (* diffuse 0. 1. 0.     , sphere  10. 10. 20. 10.; *)
                Mirror                  , sphere  20. 10. 20. 20.;
                Mirror                  , sphere  0. 0. 10. 1.;
                (* diffuse 0. 0. 1.     , sphere  0. 0. 10. 1.; *)
                color 1. 1. 1.          , sphere 20. 0. 10. 1.;
                diffuse 0. 0. 0.3       , sphere 10. ~-.1000. 0. 999.
              ];
  }

let ray_intersect_scene s r =
  List.fold_left (ray_find_min_intersection_step r) None s.objects

let rec trace_ray ?(ttl=3) ray scene =
  match ray_intersect_scene scene ray with
  | Some ((Diffuse dc, s), t) 
      when ttl > 0 -> let rt = ray.origin +| t *.| ray.dir in
		      let n = vnormalize (rt -| s.center) in
		      let add_light_color c light =
			let ray' = make_ray ~origin:rt ~dir:(light.position -| rt) () in
			match ray_intersect_scene scene ray' with
			| None -> let f = max 0. (vdot ray'.dir n) in
				  c +| f*.|(light.color*|dc)
			| Some _ -> (* We have an occluder *) c
		      in
		      List.fold_left add_light_color (scene.ambient_color *| dc) scene.lights

  | Some ((Mirror, s), t)
      when ttl > 0 -> let rt = ray.origin +| t *.| ray.dir in
		      let n = rt -| s.center in
		      let dn = ((vdot ray.dir n) /. (vdot n n)) *.| n in
		      let rdir = ray.dir -| 2.*.|dn in
		      let ray' = make_ray ~origin:rt ~dir:rdir () in
		      let ttl = ttl - 1 in
		      0.3*.|scene.ambient_color +| 0.7*.|(trace_ray ~ttl:(ttl-1) ray' scene)

  | Some ((ConstColor c, _), _) -> c

  | _ -> vzero

let render ~w ~h ~set_pixel =
  let origin = vec 0. 0. ~-.12. in
  for ix = 0 to w-1 do
    for iy = 0 to h-1 do
      let screen_pos = { x = float_of_int (2*ix-w) /. float_of_int h;
			 y = float_of_int (2*iy-h) /. float_of_int h;
			 z = ~-.10. } in

      let dir = screen_pos -| origin in
      let ray = make_ray ~origin ~dir () in

      set_pixel ix iy (trace_ray ray scene)
    done
  done

let _ =
  G.open_graph "";
  G.set_window_title "A simple raytracer in OCaml";
  ignore (G.wait_next_event [G.Key_pressed]);
  let width = G.size_x () and height = G.size_y () in
  let color_from_vec { x; y; z } =
    let r = int_of_float (255. *. x) and
	g = int_of_float (255. *. y) and
	b = int_of_float (255. *. z) in
    G.rgb (min r 255) (min g 255) (min b 255)
  in
  let set_pixel x y color_vec =
    G.set_color (color_from_vec color_vec);
    G.plot x y
  in
  render ~w:width ~h:height ~set_pixel;
  ignore (G.wait_next_event [G.Key_pressed]);
  G.close_graph ()

