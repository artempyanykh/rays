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

let ray_color _ray = (0., 0., 0.)

let raytraced_image () =
  (* Image params *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 400 in
  let image_height = float_of_int image_width /. aspect_ratio |> int_of_float in
  (* Camera params *)
  let focal_length = 1. in
  let viewport_height = 2. in
  let viewport_width =
    viewport_height *. float_of_int image_width /. float_of_int image_height
  in
  let camera_center = (0., 0., 0.) in
  (* Vectors along viewport dimensions *)
  let viewport_lr = (viewport_width, 0., 0.) in
  let viewport_ud = (0., -.viewport_height, 0.) in
  (* Delta vectors from pixel to pixel *)
  let pixel_delta_lr = Vec3d.(viewport_lr / float_of_int image_width) in
  let pixel_delta_ud = Vec3d.(viewport_ud / float_of_int image_height) in
  (* Coord of upper left pixel *)
  let viewport_upper_left =
    Vec3d.(
      camera_center - (0., 0., focal_length) - (viewport_lr / 2.)
      - (viewport_ud / 2.))
  in
  let pixel00_loc =
    Vec3d.(viewport_upper_left + (0.5 * (pixel_delta_lr + pixel_delta_ud)))
  in
  (* Render *)
  let render ~row ~col =
    let pixel_center =
      Vec3d.(
        pixel00_loc
        + (float_of_int col * pixel_delta_lr)
        + (float_of_int row * pixel_delta_ud))
    in
    let ray_dir = Vec3d.(pixel_center - camera_center) in
    let ray = Ray.{ origin = camera_center; dir = ray_dir } in
    ray_color ray |> Color.to_pixel
  in
  Image.create ~height:image_height ~width:image_width ~f:render

let () =
  let oc = Out_channel.open_text "sample.ppm" in
  try
    let fmt = Format.formatter_of_out_channel oc in
    let img = raytraced_image () in
    Format.fprintf fmt "%a" Image.pp_ppm img;
    Out_channel.close oc;
    Format.printf "Successfuly generated an image@\n"
  with ex ->
    Format.eprintf "%s" (Printexc.to_string ex);
    Out_channel.close_noerr oc
