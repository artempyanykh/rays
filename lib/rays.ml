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
