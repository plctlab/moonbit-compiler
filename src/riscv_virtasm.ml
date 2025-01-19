let outfile = Printf.sprintf "%s.deb" !Driver_config.Linkcore_Opt.output_file

let debshow (x : string) : unit =
  (* Basic_io.write outfile x *)
  print_endline @@ "[DEBUG]" ^ x

let debsexp (x : S.t) : unit =
  (* Basic_io.write_s outfile x *)
  print_endline @@ "[DEBUG]" ^ S.to_string x
