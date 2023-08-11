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

let () =
  let oc = Out_channel.open_text "sample.ppm" in
  try
    let fmt = Format.formatter_of_out_channel oc in
    let img = sample_image () in
    Format.fprintf fmt "%a" Image.pp_ppm img;
    Out_channel.close oc;
    Format.printf "Successfuly generated an image@\n"
  with ex ->
    Format.eprintf "%s" (Printexc.to_string ex);
    Out_channel.close_noerr oc
