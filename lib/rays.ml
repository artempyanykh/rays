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

module type FIELD = sig
  type t

  (* Addition operations *)
  val add : t -> t -> t
  val add_inv : t -> t
  val add_id : t

  (* Multiplication operations *)
  val mul : t -> t -> t
  val mul_inv : t -> t
  val mul_id : t
end

module type VEC_SPACE_CORE = sig
  module Scalar : FIELD

  type vec

  val vec_add : vec -> vec -> vec
  val scalar_mul : Scalar.t -> vec -> vec
end

module type VEC_SPACE = sig
  include VEC_SPACE_CORE

  val ( + ) : vec -> vec -> vec
  val ( - ) : vec -> vec -> vec
  val ( * ) : Scalar.t -> vec -> vec
  val ( / ) : vec -> Scalar.t -> vec
end

module Make_vec_space (Core : VEC_SPACE_CORE) = struct
  include Core

  let ( + ) v1 v2 = vec_add v1 v2
  let ( * ) a v = scalar_mul a v
  let ( - ) v1 v2 = vec_add v1 (Scalar.add_inv Scalar.add_id * v2)
  let ( / ) v a = scalar_mul (Core.Scalar.mul_inv a) v
end

module Vec3 (Point_core : VEC_SPACE_CORE) : sig
  include
    VEC_SPACE
      with module Scalar = Point_core.Scalar
       and type vec = Point_core.vec * Point_core.vec * Point_core.vec
end = struct
  module Point_space = Make_vec_space (Point_core)

  module Core = struct
    module Scalar = Point_core.Scalar

    type vec = Point_core.vec * Point_core.vec * Point_core.vec

    let vec_add (v11, v12, v13) (v21, v22, v23) =
      Point_space.(v11 + v21, v12 + v22, v13 + v23)

    let scalar_mul a (v1, v2, v3) = Point_space.(a * v1, a * v2, a * v3)
  end

  include Make_vec_space (Core)
end

module Vec3d = Vec3 (struct
  module Scalar = struct
    include Float

    let add_inv t = -.t
    let add_id = Float.zero
    let mul_inv t = 1. /. t
    let mul_id = Float.one
  end

  type vec = float

  let scalar_mul = ( *. )
  let vec_add = ( +. )
end)

module Ray = struct
  type t = { origin : Vec3d.vec; dir : Vec3d.vec }

  let at scale { origin; dir } = Vec3d.(origin + (scale * dir))
end

(** Color in the range [0, 1] *)
module Color = struct
  include Vec3d

  let to_pixel ((r, g, b) : vec) =
    let factor = 255.999 in
    Pixel.create
      ( int_of_float (factor *. r),
        int_of_float (factor *. g),
        int_of_float (factor *. b) )
end
