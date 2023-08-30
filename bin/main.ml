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

let ray_color (world : Shapes.hittable) (ray : Ray.t) =
  match world ray (Interval.mk 0. Float.infinity) with
  | Some hit -> Color.(0.5 * (hit.normal.vec + mk (1., 1., 1.)))
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
  (* Render *)
  let render ~(row : int) ~(col : int) =
    let pixel_center = Camera.pixel_center ~row ~col camera in
    let camera_center = Camera.center camera in
    let ray_dir = Vec3d.(pixel_center - camera_center) in
    let ray = Ray.{ origin = camera_center; dir = ray_dir } in
    ray_color world ray |> Color.to_pixel
  in
  Image.create ~height:(Camera.img_height camera) ~width:(Camera.img_width camera) ~f:render

let () =
  let img = raytraced_image () in
  Out_channel.with_open_text "sample.ppm" (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" Image.pp_ppm img);
  Format.printf "Successfuly generated an image@\n"
