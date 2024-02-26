open Core

type t = { path : string; data : Convert.t }

let setup =
  ref
    {
      path = "";
      data = { file = ""; types = []; f = { pack = ""; name = "" } };
    }

let set_path p = setup := { path = p; data = !setup.data }

let set_file f =
  setup :=
    {
      path = !setup.path;
      data = { file = f; types = !setup.data.types; f = !setup.data.f };
    }

let add_type pack name constructors =
  setup :=
    {
      path = !setup.path;
      data =
        {
          file = !setup.data.file;
          types = { pack; name; constructors } :: !setup.data.types;
          f = !setup.data.f;
        };
    }

let set_function pack name =
  setup :=
    {
      path = !setup.path;
      data =
        {
          file = !setup.data.file;
          types = !setup.data.types;
          f = { pack; name };
        };
    }

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

let quickchick_arg (x : t) =
  let coqfile = x.path ^ coqfile in
  Convert.translate (x.path ^ x.data.file) coqfile;
  Files.append_to_file coqfile (header_of_t x);
  List.iter ~f:(Tuple2.uncurry (Files.replace_in_file coqfile)) subs;
  Convert.coqc coqfile

let quickchick () = quickchick_arg !setup
