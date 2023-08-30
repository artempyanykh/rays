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
  let image_height = float_of_int image_width /. aspect_ratio |> int_of_float in
  (* World *)
  let world =
    Shapes.mk_composite
      [
        Shapes.mk_sphere (Point3d.mk (0., 0., -1.)) 0.5;
        Shapes.mk_sphere (Point3d.mk (0., -100.5, -1.)) 100.;
      ]
  in
  (* Camera params *)
  let focal_length = 1. in
  let viewport_height = 2. in
  let viewport_width =
    viewport_height *. float_of_int image_width /. float_of_int image_height
  in
  let camera_center = Vec3d.mk (0., 0., 0.) in
  (* Vectors along viewport dimensions *)
  let viewport_lr = Vec3d.mk (viewport_width, 0., 0.) in
  let viewport_ud = Vec3d.mk (0., -.viewport_height, 0.) in
  (* Delta vectors from pixel to pixel *)
  let pixel_delta_lr = Vec3d.(viewport_lr /! image_width) in
  let pixel_delta_ud = Vec3d.(viewport_ud /! image_height) in
  (* Coord of upper left pixel *)
  let viewport_upper_left =
    Vec3d.(
      camera_center
      - mk (0., 0., focal_length)
      - (viewport_lr / 2.) - (viewport_ud / 2.))
  in
  let pixel00_loc =
    Vec3d.(viewport_upper_left + (0.5 * (pixel_delta_lr + pixel_delta_ud)))
  in
  (* Render *)
  let render ~(row : int) ~(col : int) =
    let pixel_center =
      Vec3d.(pixel00_loc + (col *! pixel_delta_lr) + (row *! pixel_delta_ud))
    in
    let ray_dir = Vec3d.(pixel_center - camera_center) in
    let ray = Ray.{ origin = camera_center; dir = ray_dir } in
    ray_color world ray |> Color.to_pixel
  in
  Image.create ~height:image_height ~width:image_width ~f:render

let () =
  let img = raytraced_image () in
  Out_channel.with_open_text "sample.ppm" (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" Image.pp_ppm img);
  Format.printf "Successfuly generated an image@\n"
