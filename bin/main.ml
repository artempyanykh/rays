open Stdlib.StdLabels
open Rays

let sample_image () =
  let width, height = (256, 256) in
  let f ~row ~col =
    let r = float_of_int row /. (float_of_int @@ (height - 1)) in
    let g = float_of_int col /. (float_of_int @@ (width - 1)) in
    let b = 0.8 in
    Pixel.create
      ( int_of_float (r *. 255.999),
        int_of_float (g *. 255.99),
        int_of_float (b *. 255.999) )
  in
  Image.create ~height ~width ~f

let rec ray_color depth (world : Shapes.hittable) (ray : Ray.t) =
  if depth <= 0 then Color.black
  else
  match world ray (Interval.mk 0.001 Float.infinity) with
  | Some hit ->
      let bounce_dir = Vec3d.random_on_hemisphere ~normal:hit.normal.vec in
      let bounce_ray = Ray.{ origin = hit.point; dir = bounce_dir } in
      Color.(0.5 * ray_color (Int.sub depth 1) world bounce_ray)
  | None ->
      let unit_dir = Vec3d.unit ray.dir in
      let blend_factor = 0.5 *. (Vec3d.c2 unit_dir +. 1.) in
      Color.(
        ((1. -. blend_factor) * mk (1., 1., 1.))
        + (blend_factor * mk (0.5, 0.7, 1.0)))

let raytraced_image () =
  (* Image params *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 400 in
  let camera = Camera.mk aspect_ratio image_width in
  (* World *)
  let world =
    Shapes.mk_composite
      [
        Shapes.mk_sphere (Point3d.mk (0., 0., -1.)) 0.5;
        Shapes.mk_sphere (Point3d.mk (0., -100.5, -1.)) 100.;
      ]
  in
  let canvas =
    Array.make_matrix ~dimx:(Camera.img_height camera)
      ~dimy:(Camera.img_width camera) Color.black
  in
  (* Render *)
  let sample_ray ~row ~col =
    let pixel_sample = Camera.pixel_sample ~row ~col camera in
    let origin = Camera.center camera in
    Ray.{ origin; dir = Vec3d.(pixel_sample - origin) }
  in
  let samples_per_pixel = 10 in
  let bounce_depth = 10 in
  let render ~(row : int) ~(col : int) =
    let ray = sample_ray ~row ~col in
    canvas.(row).(col) <- Color.(canvas.(row).(col) + ray_color bounce_depth world ray)
  in
  for row = 0 to Camera.img_height camera - 1 do
    for col = 0 to Camera.img_width camera - 1 do
      for _ = 0 to samples_per_pixel - 1 do
        render ~row ~col
      done
    done
  done;
  Image.create ~height:(Camera.img_height camera)
    ~width:(Camera.img_width camera) ~f:(fun ~row ~col ->
      Color.to_pixel ~samples:samples_per_pixel canvas.(row).(col))

let () =
  let img = raytraced_image () in
  Out_channel.with_open_text "sample.ppm" (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" Image.pp_ppm img);
  Format.printf "Successfuly generated an image@\n"
