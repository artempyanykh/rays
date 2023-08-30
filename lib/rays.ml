open Stdlib.StdLabels
module F = Format

module Pixel : sig
  type t = private int

  val zero : t
  val create : int * int * int -> t
  val r : t -> int
  val g : t -> int
  val b : t -> int
  val pp_ppm : F.formatter -> t -> unit
  val to_string_ppm : t -> string
end = struct
  type t = int

  let zero = 0

  let create (r, g, b) =
    let r = (r land 255) lsl 16 in
    let g = (g land 255) lsl 8 in
    let b = b land 255 in
    r lor g lor b

  let r t = (t lsr 16) land 255
  let g t = (t lsr 8) land 255
  let b t = t land 255
  let pp_ppm fmt t = F.fprintf fmt "%3d %3d %3d" (r t) (g t) (b t)
  let to_string_ppm t = F.asprintf "%a" pp_ppm t
end

module Image = struct
  type t = { pixels : Pixel.t array array }

  let create ~height ~width ~f =
    let pixels = Array.make_matrix ~dimx:height ~dimy:width Pixel.zero in
    for row = 0 to height - 1 do
      for col = 0 to width - 1 do
        pixels.(row).(col) <- f ~row ~col
      done
    done;
    { pixels }

  let pp_ppm fmt { pixels } =
    let num_rows = Array.length pixels in
    let num_cols = Array.length pixels.(0) in
    F.fprintf fmt "P3@\n%d %d@\n255@\n" num_cols num_rows;
    for row = 0 to num_rows - 1 do
      for col = 0 to num_cols - 1 do
        F.fprintf fmt "%a@\n" Pixel.pp_ppm pixels.(row).(col)
      done
    done
end

module Vec3d : sig
  type t = private float * float * float

  val mk : float * float * float -> t
  val raw : t -> float * float * float
  val c1 : t -> float
  val c2 : t -> float
  val c3 : t -> float
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : float -> t -> t
  val ( *! ) : int -> t -> t
  val ( / ) : t -> float -> t
  val ( /! ) : t -> int -> t
  val length_square : t -> float
  val unit : t -> t
  val dot : t -> t -> float
  val random_unit : unit -> t
  val random_on_hemisphere : normal:t -> t
end = struct
  type t = float * float * float

  let mk v = v
  let raw v = v
  let c1 (v1, _, _) = v1
  let c2 (_, v2, _) = v2
  let c3 (_, _, v3) = v3

  let ( + ) (v11, v12, v13) (v21, v22, v23) =
    (v11 +. v21, v12 +. v22, v13 +. v23)

  let ( - ) (v11, v12, v13) (v21, v22, v23) =
    (v11 -. v21, v12 -. v22, v13 -. v23)

  let ( ~- ) (a, b, c) = (-.a, -.b, -.c)
  let ( * ) s (v1, v2, v3) = (s *. v1, s *. v2, s *. v3)
  let ( *! ) is v = float_of_int is * v
  let ( / ) (v1, v2, v3) s = (v1 /. s, v2 /. s, v3 /. s)
  let ( /! ) v is = v / float_of_int is
  let length_square (v1, v2, v3) = (v1 *. v1) +. (v2 *. v2) +. (v3 *. v3)
  let length v = sqrt (length_square v)
  let unit v = v / length v
  let dot (a1, a2, a3) (b1, b2, b3) = (a1 *. b1) +. (a2 *. b2) +. (a3 *. b3)

  let random ?(min = 0.) ?(max = 1.) () =
    let max = max -. 0.00000001 in
    let rand () = Random.float (max -. min) +. min in
    mk (rand (), rand (), rand ())

  let rec random_in_unit_sphere () =
    let v = random ~min:(-1.) ~max:1. () in
    if length_square v < 1. then v else random_in_unit_sphere ()

  let random_unit () = unit (random_in_unit_sphere ())

  let random_on_hemisphere ~normal =
    let v = random_unit () in
    if dot v normal > 0. then v else -v
end

module Point3d = Vec3d

module Ray = struct
  type t = { origin : Point3d.t; dir : Vec3d.t }

  let at scale { origin; dir } = Vec3d.(origin + (scale * dir))
end

(** Color in the range [0, 1] *)
module Color = struct
  include Vec3d

  let black = mk (0., 0., 0.)

  let to_pixel ?(samples = 1) t =
    let factor = 255.999 in
    let r, g, b = raw (factor * t /! samples) in
    Pixel.create (int_of_float r, int_of_float g, int_of_float b)
end

module Interval = struct
  type t = { tmin : float; tmax : float }

  let empty = { tmin = Float.infinity; tmax = Float.neg_infinity }
  let universe = { tmin = Float.neg_infinity; tmax = Float.infinity }
  let mk tmin tmax = { tmin; tmax }
  let contains x { tmin; tmax } = tmin <= x && x <= tmax
  let surrounds x { tmin; tmax } = tmin < x && x < tmax

  let clamp x { tmin; tmax } =
    if x < tmin then tmin else if x > tmax then tmax else x
end

module Shapes : sig
  type face = Front | Back
  type normal = { vec : Vec3d.t; face : face }
  type hit = { point : Point3d.t; normal : normal; t : float }
  type hittable = Ray.t -> Interval.t -> hit option

  val mk_sphere : Point3d.t -> float -> hittable
  val mk_composite : hittable list -> hittable
end = struct
  type face = Front | Back
  type normal = { vec : Vec3d.t; face : face }
  type hit = { point : Point3d.t; normal : normal; t : float }
  type hittable = Ray.t -> Interval.t -> hit option

  let mk_sphere center radius : hittable =
    let r2 = radius *. radius in
    fun (ray : Ray.t) (interval : Interval.t) ->
      let oc = Vec3d.(ray.origin - center) in
      let a = Vec3d.length_square ray.dir in
      let half_b = Vec3d.dot oc ray.dir in
      let c = Vec3d.length_square oc -. r2 in
      let discriminant = (half_b *. half_b) -. (a *. c) in
      if discriminant < 0. then None
      else
        let sqrtd = sqrt discriminant in
        let mk_hit t =
          let point = Ray.at t ray in
          let outward_normal = Vec3d.((point - center) / radius) in
          let normal =
            if Vec3d.dot ray.dir outward_normal < 0. then
              { vec = outward_normal; face = Front }
            else { vec = Vec3d.(-outward_normal); face = Back }
          in
          { point; normal; t }
        in
        let root = (-.half_b -. sqrtd) /. a in
        if Interval.surrounds root interval then Some (mk_hit root)
        else
          let root = (-.half_b +. sqrtd) /. a in
          if Interval.surrounds root interval then Some (mk_hit root) else None

  let mk_composite (objects : hittable list) : hittable =
   fun ray interval ->
    List.fold_left ~init:None objects ~f:(fun closest obj ->
        let tmax =
          Option.map (fun ({ t; _ } : hit) -> t) closest
          |> Option.value ~default:interval.tmax
        in
        match obj ray (Interval.mk interval.tmin tmax) with
        | None -> closest
        | hit -> hit)
end

module Camera : sig
  type t

  val mk : float -> int -> t
  val img_width : t -> int
  val img_height : t -> int
  val aspect : t -> float
  val center : t -> Point3d.t
  val pixel_center : row:int -> col:int -> t -> Point3d.t
  val pixel_sample : row:int -> col:int -> t -> Point3d.t
end = struct
  type derived_state = {
    img_height : int;
    pixel00 : Point3d.t;
    pixel_delta_lr : Vec3d.t;
    pixel_delta_ud : Vec3d.t;
  }

  type t = {
    aspect : float;
    img_width : int;
    center : Point3d.t;
    derived : derived_state;
  }

  let mk aspect img_width =
    let img_height = float_of_int img_width /. aspect |> int_of_float in
    let focal_length = 1. in
    let viewport_height = 2. in
    let viewport_width =
      viewport_height *. float_of_int img_width /. float_of_int img_height
    in
    let camera_center = Vec3d.mk (0., 0., 0.) in
    (* Vectors along viewport dimensions *)
    let viewport_lr = Vec3d.mk (viewport_width, 0., 0.) in
    let viewport_ud = Vec3d.mk (0., -.viewport_height, 0.) in
    (* Delta vectors from pixel to pixel *)
    let pixel_delta_lr = Vec3d.(viewport_lr /! img_width) in
    let pixel_delta_ud = Vec3d.(viewport_ud /! img_height) in
    (* Coord of upper left pixel *)
    let viewport_upper_left =
      Vec3d.(
        camera_center
        - mk (0., 0., focal_length)
        - (viewport_lr / 2.) - (viewport_ud / 2.))
    in
    let pixel00 =
      Vec3d.(viewport_upper_left + (0.5 * (pixel_delta_lr + pixel_delta_ud)))
    in
    let derived = { img_height; pixel00; pixel_delta_lr; pixel_delta_ud } in
    { aspect; img_width; center = camera_center; derived }

  let pixel_center ~row ~col camera =
    Point3d.(
      camera.derived.pixel00
      + (col *! camera.derived.pixel_delta_lr)
      + (row *! camera.derived.pixel_delta_ud))

  let pixel_sample ~row ~col camera =
    let upper_bound = 0.999999 in
    let px = -0.5 +. Random.float upper_bound in
    let py = -0.5 +. Random.float upper_bound in
    Vec3d.(
      (px * camera.derived.pixel_delta_lr)
      + (py * camera.derived.pixel_delta_ud)
      + pixel_center ~row ~col camera)

  let img_width { img_width; _ } = img_width
  let img_height { derived = { img_height; _ }; _ } = img_height
  let aspect { aspect; _ } = aspect
  let center { center; _ } = center
end
