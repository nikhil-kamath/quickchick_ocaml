open Core

type t = { path : string; data : Convert.t }

let coqfile = "_temp_quickchick_ocaml.v"

(* coq-of-ocaml does some things which i dont like so we will be overriding them *)
let subs = [ ("int", "nat") ]

let header_of_t (t : t) =
  List.concat
    [
      [
        "From QuickChick Require Import QuickChick.";
        "QCInclude \"" ^ t.path ^ t.data.file ^ "\".";
      ];
      Core.List.concat_map ~f:Convert.header_of_ty t.data.types;
      Convert.header_of_func t.data.f;
      [ "QuickChick " ^ t.data.f.name ^ "." ];
    ]

let quickchick (x : t) =
  let coqfile = x.path ^ coqfile in
  Convert.translate (x.path ^ x.data.file) coqfile;
  Files.append_to_file coqfile (header_of_t x);
  List.iter ~f:(Tuple2.uncurry (Files.replace_in_file coqfile)) subs;
  Convert.coqc coqfile
