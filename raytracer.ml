(* A simple OCaml raytracer with phong-shading, reflections and
   shadows in around 200 lines *)


(* First, let us define a 3D vector in euclidean space *)

type vec_t = { x : float;
	       y : float;
	       z : float }

let ( +| ) a b	= { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
let ( -| ) a b	= { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
let ( *| ) a b	= { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z }
let ( /| ) a b	= { x = a.x /. b.x; y = a.y /. b.y; z = a.z /. b.z }
let ( *.| ) s a	= { x = s *. a.x; y = s *. a.y; z = s *. a.z }

let vec x y z	= { x; y; z }
let vone	= vec 1. 1. 1.
let vzero	= vec 0. 0. 0.

let vdot a b		= a.x*.b.x +. a.y*.b.y +. a.z*.b.z
let vlen_sq a		= vdot a a
let vlen a		= sqrt (vlen_sq a)
let vnormalize a	= (1./.vlen a) *.| a
let vdist_sq a b	= vlen_sq (a -| b)
let vdist a b		= vlen (a -| b)

let vcross a b		= { x = a.y *. b.z -. a.z *. b.y;
			    y = a.z *. b.x -. a.x *. b.z;
			    z = a.x *. b.y -. a.y *. b.x }
let vproject a b	= ((vdot a b) /. (vdot b b)) *.| b
let vreflect a n	= a -| 2.*.|(vproject a n)

(* Next, a ray *)

type ray_t = { origin : vec_t; dir : vec_t }

let make_ray ?(origin=vzero) ~dir () =
  { origin;
    dir = vnormalize dir }

let ray_eval r t = r.origin +| t *.| r.dir

(* A sphere *)

type sphere_t = { center : vec_t; radius : float }

let make_sphere x y z ~r = { center=vec x y z; radius=r }

type material_t = {
  ambient : vec_t;
  diffuse : vec_t;
  specular : vec_t;
  shininess : float;
  reflection : float
}

type object_t = material_t * sphere_t
type intersection_t = (object_t * float) option

let object_normal_on_surface (_, s) sp =
  vnormalize (sp -| s.center)

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

let ray_intersect_scene s r =
  List.fold_left (ray_find_min_intersection_step r) None s.objects

let trace_max_depth = 10

let rec trace_ray ?(depth=0) scene ray =
  match ray_intersect_scene scene ray with
  | Some ((mat, s) as obj, t) when depth < trace_max_depth ->
    let pos = ray_eval ray t  in
    let n = object_normal_on_surface obj pos in
    let add_light c_sum light =
      let ray' = make_ray ~origin:pos ~dir:(light.position -| pos) () in
      if ray_intersect_scene scene ray' = None then
	let f_diff = max 0. (vdot ray'.dir n) in
	let f_spec = max 0. (vdot (vreflect ray'.dir n) ray.dir) in
	let c_diff = f_diff *.| mat.diffuse *| light.color in
	let c_spec = (f_spec ** mat.shininess) *.| mat.specular *| light.color in
	c_sum +| c_diff +| c_spec
      else
	c_sum
    in
    let color =
      List.fold_left
	add_light
	(mat.ambient *| scene.ambient_color)
	scene.lights
    in
    let k_r = mat.reflection in
    if k_r > 0. then
      let dir' = vreflect ray.dir n in
      let ray' = make_ray ~origin:pos ~dir:dir' () in
      k_r*.|(trace_ray ~depth:(depth+1) scene ray') +| (1.-.k_r)*.|color
    else
      color

  | Some ((mat, _), _) -> mat.ambient *| scene.ambient_color

  | _ -> vzero

let scene =
  let diffuse r g b = {
    ambient = vzero;
    diffuse = vec r g b;
    specular = 0.3 *.| vone;
    shininess = 50.;
    reflection = 0.
  } in
  let mirror = {
    ambient = vone;
    diffuse = vzero;
    specular = vone;
    shininess = 100.;
    reflection = 0.5
  } in
  let diffuse_white = diffuse 1. 1. 1. in
  let sphere x y z r	= make_sphere x y z ~r in
  {
    ambient_color = vec 0.2 0.2 0.2;
    lights = [
      make_light 0. 100. ~-.100.      	1. 1. 1.;
      make_light ~-.10. 10. 2.		0.5 0.5 0.;
      make_light 0. 8. 8.		0. 0. 1.
    ];
    objects = [
      mirror       , sphere 10. ~-.1000. 0. 999.;
      diffuse_white		, sphere  4. 4. 2. 1.;
      diffuse_white		, sphere  2. 4. 2. 1.;
      diffuse_white		, sphere  4. 2. 2. 1.;
      diffuse_white		, sphere  2. 2. 2. 1.;
      diffuse_white        	, sphere  4. 4. 4. 1.;
      diffuse_white        	, sphere  2. 4. 4. 1.;
      diffuse_white	   	, sphere  4. 2. 4. 1.;
      diffuse_white	        , sphere  2. 2. 4. 1.;
      (* diffuse 0. 1. 0.     , sphere  10. 10. 20. 10.; *)
      mirror                  , sphere  20. 10. 20. 20.;
      mirror                  , sphere  0. 0. 10. 1.;
    (* diffuse 0. 0. 1.     , sphere  0. 0. 10. 1.; *)
    ];
  }

let driver ~w ~h ~set_pixel ~tracer =
  let origin = vec 0. 0. ~-.12. in
  for ix = 0 to w-1 do
    for iy = 0 to h-1 do
      let screen_pos = { x = float_of_int (2*ix-w) /. float_of_int h;
			 y = float_of_int (2*iy-h) /. float_of_int h;
			 z = ~-.10. } in

      let dir = screen_pos -| origin in
      let ray = make_ray ~origin ~dir () in

      let color = tracer ray in
      set_pixel ~x:ix ~y:iy ~r:color.x ~g:color.y ~b:color.z
    done
  done

let _ = Win.render_and_display (driver ~tracer:(trace_ray scene))
