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

let make_sphere x y z ~r = { center=vec x y z; radius=r }





















(* A scene object with some unspecified attributes *)

type 'a object_t = 'a * sphere_t


(* Intersection of a scene object with a ray *)

type 'a intersection_t =
  | Passthrough
  | Intersect of 'a object_t * float


(* Object / sphere normal *)

let object_normal_on_surface (_, s) sp =
  vnormalize (sp -| s.center)


(* Object / sphere intersection *)

let ray_intersect_object r ((_, s) as obj) =
  (* Solve quadratic equation:

     distance(r.dir*t, center') = s.radius
  *)
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





















(* We start with a simple sphere and some colors*)

let obj = ((), make_sphere 0. 0. 10. 2.)

let color_white = vec 1. 1. 1.
let color_black = vec 0. 0. 0.
let color_red	= vec 1. 0. 0.
let color_green = vec 0. 1. 0.
let color_blue	= vec 0. 0. 1.


(* Test our object intersection algorithm *)

let trace ray =
  match ray_intersect_object ray obj with
  | Passthrough -> color_black
  | Intersect _ -> color_white

let _ = render trace



















(* Seems to work, let's add color. Remember 'a object_t? *)

let obj = (vec 1. 1. 0., make_sphere 0. 0. 10. 2.)

let trace ray =
  match ray_intersect_object ray obj with
  | Passthrough -> color_black
  | Intersect ((c,_), _) -> c

let _ = render trace

(* Ok. But what about more than one sphere? *)





















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

let rec ray_intersect_objects' r i_min = function
  | [] -> i_min
  | o::rest -> let i = ray_intersect_object r o in
	       let i_min = intersection_min i i_min in
	       ray_intersect_objects' r i_min rest


(* In the beginning, we have no intersection, i.e. Passthrough *)

let ray_intersect_objects r objects =
  ray_intersect_objects' r Passthrough objects


(* We can rewrite the above using higher order functions *)

let ray_intersect_objects r objects =
  let intersect' i o = intersection_min i (ray_intersect_object r o) in
  List.fold_left intersect' Passthrough objects

(* Let's test it out again *)




















(* A few objects *)

let objs = 
  let a = ~-.1.5 and b = 1.5 in [
    color_red,		make_sphere a a 10. 2.;
    color_green,	make_sphere a b 10. 2.;
    color_blue,		make_sphere b a 10. 2.;
    color_white,	make_sphere b b 10. 2.;
  ]


(* Still all very simple *)

let trace ray =
  match ray_intersect_objects ray objs with
  | Passthrough -> color_black
  | Intersect ((c,_), _) -> c

let _ = render trace





















(* Good, multiple objects work as well. Let's add some simple diffuse
   shading with a single directional light in view direction. *)

let trace ray =
  match ray_intersect_objects ray objs with
  | Passthrough -> color_black
  | Intersect ((c,_) as o, t) -> let pos = ray_eval ray t in
				 let n = object_normal_on_surface o pos in
				 let light_dir = ray.dir in
				 let f = max 0. ~-.(vdot light_dir n) in
				 f *.| c

let _ = render trace

(* Nice, let's add more than one light and shadows! *)





















(* A simple positional light without attenuation *)

type light_t = {
  color		: vec_t;
  position	: vec_t;
}


(* Conveniently create new lights *)

let make_light ?(color=color_white) ?(position=vzero) () =
  { color; position }






















(* It's a good idea to pass the objects and lights explicitly as
   arguments. *)

let trace_objects objs lights ray =
  match ray_intersect_objects ray objs with
  | Passthrough ->
    color_black

  | Intersect ((color,_) as o, t) ->
    let pos = ray_eval ray t in
    let n = object_normal_on_surface o pos in

    (* Accumulate contribution ... *)
    let add_light c_sum light =
      let dir' = light.position -| pos in
      let ray' = make_ray ~origin:pos ~dir:dir' () in
      (* Check if light is occluded or not *)
      if ray_intersect_objects ray' objs = Passthrough then
	let f = max 0. (vdot ray'.dir n) in
	c_sum +| f*.|color*|light.color
      else
	c_sum
    in
    (* ... for each lights *)
    List.fold_left add_light color_black lights





















(* A few colored lights and a single white object with ground below it *)

let objs = [
  color_white,	make_sphere 0. 0. 10. 4.;
  color_white,	make_sphere 0. ~-.1000. 0. 999.;
]

let lights =
  let a = ~-.10. and b = 10. in [
    make_light ~position:(vec a  b  0.) ~color:color_red ();
    make_light ~position:(vec b  b  0.) ~color:color_green ();
    make_light ~position:(vec 0. 0. 0.) ~color:color_blue ()
  ]


(* Rendering ... *)

let _ = render (trace_objects objs lights)





















(* Something seems to be wrong... *)

let _ = ()

(* The shadow ray intersects the ground far after the light. We need
   only a clear line to the light, not to infinity and beyond. *)





















(* Let's make sure we only check the interval between the surface and
   the light for occluders. *)

let intersection_t = function
  | Passthrough -> infinity
  | Intersect (_, t) -> t


(* Try again! *)

let trace_objects objects lights ray =
  match ray_intersect_objects ray objs with
  | Passthrough ->
    color_black

  | Intersect ((color,_) as o, t) ->
    let pos = ray_eval ray t in
    let n = object_normal_on_surface o pos in

    let add_light c_sum light =
      let dir' = light.position -| pos in
      let ray' = make_ray ~origin:pos ~dir:dir' () in
      (* Check if light is occluded or not *)
      let t = intersection_t (ray_intersect_objects ray' objs) in
      (* Occluder between light and surface *)
      if t < vdot dir' ray'.dir then
	c_sum
      else
	let f = max 0. (vdot ray'.dir n) in
	c_sum +| f*.|color*|light.color
    in

    List.fold_left add_light color_black lights





















(* Fingers crossed... *)

let _ = render (trace_objects objs lights) 





















(* Whew. *)

let _ = ()

(* But every raytracer needs reflections! We can implement those
   by making our trace function recursive. *)



















(* Our trace function will become recursive ... *)

let max_depth = 5

(* ... so an upper limit on the recursion depth is a good idea!
   Considering halls of mirrors and everything... *)

let rec trace_objects objs lights ?(depth=0) ray =
  match ray_intersect_objects ray objs with
  | Passthrough ->
    color_black

  | Intersect ((color,_) as o, t) ->
    let pos = ray_eval ray t in
    let n = object_normal_on_surface o pos in

    let add_light c_sum light =
      let dir' = light.position -| pos in
      let ray' = make_ray ~origin:pos ~dir:dir' () in
      (* Check if light is occluded or not *)
      let t = intersection_t (ray_intersect_objects ray' objs) in
      (* Occluder between light and surface *)
      if t < vdot dir' ray'.dir then
	c_sum
      else
	let f = max 0. (vdot ray'.dir n) in
	c_sum +| f*.|color*|light.color
    in
    let color = List.fold_left add_light color_black lights in
    let dir' = vreflect ray.dir n in
    let ray' = make_ray ~origin:pos ~dir:dir' () in
    if depth < max_depth then
      (* Half-half *)
      0.5*.|(trace ~depth:(depth+1) ray') +| 0.5*.|color
    else
      color




















(* It's time for some reflections! *)

let objs = [
  color_red,	make_sphere ~-.8. 2. 8. 4.;
  color_white,	make_sphere    0. 2. 10. 4.;
  color_green,	make_sphere    8. 2. 8. 4.;
  color_white,	make_sphere 0. ~-.1000. 0. 998.;
]

let lights = [
  make_light ()
]

let _ = render (trace_objects objs lights)




















    
(* Time to get a little more sophisticated with the shading. Full
   phong shading style! *)

let _ = ()





















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





















(* Define some materials *)

let mat_white	= make_material ()
let mat_red	= make_material ~diffuse:color_red ()
let mat_green	= make_material ~diffuse:color_green ()
let mat_blue	= make_material ~diffuse:color_blue ()
let mat_mirror  = make_material ~reflection:0.5 ()

(* and a scene *)

let scene = 
  let ambient_color = 0.2 *.| vone in
  let lights =
    let a = ~-.10. and b = 10. in [
      make_light ();
      make_light ~position:(vec a  b b) ~color:color_red ();
      make_light ~position:(vec b  b b) ~color:color_green ();
      make_light ~position:(vec 0. b b) ~color:color_blue ()
    ]
  in
  let objects =
    let a = ~-.1.5 and b = 1.5 in [
      mat_red,		make_sphere a a 10. 2.;
      mat_green,	make_sphere a b 10. 2.;
      mat_blue,		make_sphere b a 10. 2.;
      mat_white,	make_sphere b b 10. 2.;
      mat_mirror,	make_sphere 0. ~-.1000. 0. 997.;
    ]
  in
  { ambient_color; lights; objects }

(* And go! *)

let _ = render (trace_scene scene)

