(* A simple OCaml raytracer with phong-shading, reflections and
   shadows. *)



(* First, let us define a 3D vector in euclidean space and the
   usual operators and functions *)

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

let vproject a b	= ((vdot a b) /. (vdot b b)) *.| b
let vreflect a n	= a -| 2.*.|(vproject a n)
let vcross a b		= { x = a.y *. b.z -. a.z *. b.y;
			    y = a.z *. b.x -. a.x *. b.z;
			    z = a.x *. b.y -. a.y *. b.x }

(* Next, a ray *)

type ray_t = { origin : vec_t; dir : vec_t }

let make_ray ?(origin=vzero) ~dir () =
  { origin;
    dir = vnormalize dir }

let ray_eval r t = r.origin +| t *.| r.dir

(* A sphere *)

type sphere_t = { center : vec_t;
		  radius : float }

let make_sphere x y z r = { center=vec x y z; radius=r }

(* A scene object with some unspecified attributes *)

type 'a object_t = 'a * sphere_t

(* Intersection of a scene object with a ray *)

type 'a intersection_t =
  | Passthrough
  | Intersect of 'a object_t * float

(* Object / sphere normal *)

let object_normal_on_surface (_, s) sp =
  vnormalize (sp -| s.center)

(* Object / sphere intersection

   Solve quadratic equation:

   distance(r.dir*t, center') = s.radius
*)
let ray_intersect_object r ((_, s) as obj) =
  let o = r.origin -| s.center and
      d = r.dir in
  let a = vdot d d and
      b = 2. *. vdot o d and
      c = vdot o o -. s.radius*.s.radius in
  let d = b*.b -. 4.*.a*.c in
  if d < 0. then
    (* No intersection *)
    Passthrough
  else
    let t =
      if a < 0. then
	(~-.b +. sqrt d)/.(2.*.a)
      else
	(~-.b -. sqrt d)/.(2.*.a)
    in
    if t < 0.0 then
      (* Intersection behind ray *)
      Passthrough
    else
      Intersect (obj, t)

(* We need a simple test function to visualize our trace functions *)

let render tracer =
  let driver ~w ~h ~set_pixel ~tracer =
    (* Eye is at (0,0,0), screen is at roughly (-1,-1,2)-(1,1,2) *)
    let origin = vec 0. 0. 0. in
    (* Shoot rays for all the pixels *)
    for ix = 0 to w-1 do
      for iy = 0 to h-1 do
	let screen_pos = { x = float_of_int (2*ix-w) /. float_of_int h;
			   y = float_of_int (2*iy-h) /. float_of_int h;
			   z = 2. } in

	let dir = screen_pos -| origin in
	let ray = make_ray ~origin ~dir () in

	let color = tracer ray in
	set_pixel ~x:ix ~y:iy ~r:color.x ~g:color.y ~b:color.z
      done
    done
  in
  Win.render_and_display (driver ~tracer)

(* Given two intersections, find the one closer to the origin. This is
   a perfect match for pattern matching (pun intended) *)

let intersection_min i1 i2 = 
  match i1, i2 with
  | Passthrough, _ -> i2
  | _, Passthrough -> i1
  | Intersect (_,t1), Intersect (_,t2) when t1 < t2 -> i1
  | _ -> i2

(* Recursively reduce the list of objects until base case (no objects)
   is reached. Return the min intersection *)

let ray_intersect_objects r objects =
  let intersect' i o = intersection_min i (ray_intersect_object r o) in
  List.fold_left intersect' Passthrough objects

(* A simple positional light without attenuation *)

type light_t = {
  color		: vec_t;
  position	: vec_t;
}

(* Conveniently create new lights *)

let color_white = vec 1. 1. 1.
let color_black = vec 0. 0. 0.
let color_red	= vec 1. 0. 0.
let color_green = vec 0. 1. 0.
let color_blue	= vec 0. 0. 1.

let make_light ?(color=color_white) ?(position=vzero) () =
  { color; position }
(* Let's make sure we only check the interval between the surface and
   the light for occluders. *)

let intersection_t = function
  | Passthrough -> infinity
  | Intersect (_, t) -> t

(* Material parameters *)

type material_t = {
  ambient	: vec_t;
  diffuse	: vec_t;
  specular	: vec_t;
  shininess	: float;
  reflection	: float;
}

(* Easily make new materials, this is where optional parameters start
   to shine. *)

let make_material
    ?(ambient=vone)
    ?(diffuse=vone)
    ?(specular=0.3*.|vone)
    ?(shininess=50.)
    ?(reflection=0.)
    () =
  { ambient; diffuse; specular; shininess; reflection }

(* A scene holds everything together *)

type scene_t = {
  ambient_color : vec_t;
  lights	: light_t list;
  objects	: material_t object_t list;
}

(* Still unterstandable, no? *)

let max_depth = 5

let rec trace_scene ?(depth=0) scene ray =
  match ray_intersect_objects ray scene.objects with
  | Passthrough ->
    color_black

  | Intersect ((mat,_) as o, t) ->
    let pos = ray_eval ray t in
    let n = object_normal_on_surface o pos in
    let add_light c_sum light =
      let dir' = light.position -| pos in
      let ray' = make_ray ~origin:pos ~dir:dir' () in
      (* Check if light is occluded or not *)
      let t = intersection_t (ray_intersect_objects ray' scene.objects) in
      (* Occluder between light and surface? *)
      if t < vdot dir' ray'.dir then
	c_sum
      else
	(* Add specular and diffuse lighting... *)
	let f_diff = max 0. (vdot ray'.dir n) in
	let f_spec = max 0. (vdot (vreflect ray'.dir n) ray.dir) in
	let c_diff = f_diff *.| mat.diffuse *| light.color in
	let c_spec = f_spec**mat.shininess *.| mat.specular *| light.color in
	c_sum +| c_diff +| c_spec
    in
    (* ... and ambient lighting as well *)
    let c_ambi = scene.ambient_color*|mat.ambient in
    let color = List.fold_left add_light c_ambi scene.lights in
    (* Add reflection if necessary *)
    let k_r = mat.reflection in
    if k_r > 0. && depth < max_depth then
      let dir' = vreflect ray.dir n in
      let ray' = make_ray ~origin:pos ~dir:dir' () in
      k_r*.|(trace_scene ~depth:(depth+1) scene ray') +| (1.-.k_r)*.|color
    else
      color

(* Bonus materials *)

let mat_white	= make_material ~diffuse:(vec 0.7 0.7 0.7) ()
let mat_red	= make_material ~diffuse:(vec 1.0 0.2 0.5) ()
let mat_green	= make_material ~diffuse:(vec 0.5 1.0 0.2) ()
let mat_blue	= make_material ~diffuse:(vec 0.2 0.5 1.0) ()
let mat_mirror  = make_material ~specular:vone ~reflection:0.5 ~diffuse:vzero ()

(* Bonus scene *)

let scene = 
  let ambient_color = vec 0.1 0.1 0.1 in
  let lights =
    let a = ~-.10. and b = 10. and c = 30. in [
      make_light ();
      make_light ~position:(vec a  b c) ~color:(vec 0.5 0.1 0.1) ();
      make_light ~position:(vec b b c) ~color:(vec 0.1 0.5 0.1) ();
      make_light ~position:(vec 0. b c) ~color:(vec 0.1 0.1 0.5) ()
    ]
  in
  let objects =
    let z = 60. and a = ~-.10. and b = 10. in [
      mat_red,		
      make_sphere 0. 4. z 4.;
      mat_green,	
      make_sphere 0. 11. z 3.5;
      mat_blue,		
      make_sphere 0. 17. z 3.;
      mat_white,	
      make_sphere 0. 22. z 2.5;
      mat_mirror, 
      make_sphere 0. ~-.1000. 0. 997.;
      mat_white,
      make_sphere 0. ~-.30. z 30.5;
      mat_white,
      make_sphere 30. ~-.37. (z+.10.) 40.5;
      mat_white,
      make_sphere ~-.35. ~-.35. (z+.10.) 45.5;
      mat_white,
      make_sphere ~-.10. ~-.120. (z+.100.) 150.;
      mat_white,
      make_sphere 60. ~-.120. (z+.130.) 160.;
      mat_white,
      make_sphere ~-.60. ~-.100. (z+.260.) 200.;
      mat_mirror,
      make_sphere ~-.10. ~-.2.8 15. 2.;
      mat_mirror,
      make_sphere 5. ~-.7. 15. 5.;
      mat_mirror,
      make_sphere 10. ~-.8. 20. 7.;
     ]
  in
  { ambient_color; lights; objects }

let _ = render (trace_scene scene)
