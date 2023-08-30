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
end

module Point3d = Vec3d

module Ray = struct
  type t = { origin : Point3d.t; dir : Vec3d.t }

  let at scale { origin; dir } = Vec3d.(origin + (scale * dir))
end

(** Color in the range [0, 1] *)
module Color = struct
  include Vec3d

  let to_pixel t =
    let factor = 255.999 in
    let r, g, b = raw (factor * t) in
    Pixel.create (int_of_float r, int_of_float g, int_of_float b)
end

module Shapes : sig
  type face = Front | Back
  type normal = { vec : Vec3d.t; face : face }
  type hit = { point : Point3d.t; normal : normal; t : float }
  type hittable = Ray.t -> tmin:float -> tmax:float -> hit option

  val mk_sphere : Point3d.t -> float -> hittable
  val mk_composite : hittable list -> hittable
end = struct
  type face = Front | Back
  type normal = { vec : Vec3d.t; face : face }
  type hit = { point : Point3d.t; normal : normal; t : float }
  type hittable = Ray.t -> tmin:float -> tmax:float -> hit option

  let mk_sphere center radius : hittable =
    let r2 = radius *. radius in
    fun (ray : Ray.t) ~tmin ~tmax ->
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
        if root <= tmin || root >= tmax then
          let root = (-.half_b +. sqrtd) /. a in
          if root <= tmin || root >= tmax then None else Some (mk_hit root)
        else Some (mk_hit root)

  let mk_composite (objects : hittable list) : hittable =
   fun ray ~tmin ~tmax ->
    List.fold_left ~init:None objects ~f:(fun closest obj ->
        let tmax =
          Option.map (fun ({ t; _ } : hit) -> t) closest
          |> Option.value ~default:tmax
        in
        match obj ray ~tmin ~tmax with None -> closest | hit -> hit)
end
