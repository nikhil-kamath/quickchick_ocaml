(*
  Functions for running quickchick on OCaml via Coq translation.
  This should go hand-in-hand with a preprocessor which is WIP
*)
let run cmd =
  let inp = Core_unix.open_process_in cmd in
  let r = Core.In_channel.input_lines inp in
  Core.In_channel.close inp;
  r

(** `translate i o` calls `coq-of-ocaml i -output=o` *)
let translate (i : string) (o : string) =
  run ("coq-of-ocaml " ^ i ^ " -output=" ^ o) |> ignore

(** `coqc f` calls `coqc f -w "-all" ` and returns a list of strings which the compilation results in  *)
let coqc_raw (f : string) = run ("coqc " ^ f ^ " -w \"-all\"")

let coqc (f : string) =
  run ("coqc " ^ f ^ " -w \"-all\"") |> Files.find_time_elapsed

type ty = { pack : string; name : string; constructors : string list }
(**
Mimicks necessary information to extract an OCaml type into Coq

For example, if a type `bar` is defined in `prop.ml`, the necessary instance would be
  { pack = "Prop"; name = "bar"; constructors = [ "A"; "B" ] }
*)

type func = { pack : string; name : string }
(**
Mimicks necessary information to extract an OCaml function name into Coq

For example, if a function `foo` is defined in `prop.ml`, the necessary instance would be
  { pack = "Prop"; name = "foo" }
*)

type t = { file : string; types : ty list; f : func }

(** reference_of "Module" "foo" returns an escaped string "Module.foo" *)
let reference_of pack name = "\"" ^ pack ^ "." ^ name ^ "\""

let ( @ ) = reference_of

let header_of_ty (s : ty) =
  List.concat
    [
      [
        "Derive (Show, Arbitrary) for " ^ s.name ^ ".";
        "Extract Inductive " ^ s.name ^ " => " ^ s.pack @ s.name;
        "  [";
      ];
      Core.List.map ~f:(fun c -> "    " ^ s.pack @ c) s.constructors;
      [ "  ]." ];
    ]

let header_of_func (f : func) =
  [ "Extract Constant " ^ f.name ^ " => " ^ (f.pack @ f.name) ^ "." ]
