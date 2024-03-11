(* Contains command-line based tools for generating initial coq files. *)

let run_command cmd =
  let inp = Core_unix.open_process_in cmd in
  let r = Core.In_channel.input_lines inp in
  Core.In_channel.close inp;
  r

let run_coq_of_ocaml infile outfile =
  run_command (Printf.sprintf "coq-of-ocaml %s -output=%s" infile outfile)
  |> ignore

let run_coqc dir coqfile =
  run_command (Printf.sprintf "cd %s; coqc %s  -w \"-all\"" dir coqfile)


