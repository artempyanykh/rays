open Rays
module F = Format

let%expect_test _ =
  let p = Pixel.create (1, 2, 3) in
  F.printf "%a" Pixel.pp_ppm p;
  [%expect {| 1   2   3 |}]

let%expect_test _ =
  let img =
    Image.create ~width:3 ~height:2 ~f:(fun ~row ~col ->
        Pixel.create (row, col, row * col))
  in
  F.printf "%a" Image.pp_ppm img;
  [%expect
    {|
    P3
    3 2
    255
      0   0   0   0   1   0   0   2   0
      1   0   0   1   1   1   1   2   2 |}]
