(* types and other important input specifications *)
module Extractions = struct
  (* extract types *)
  type ty = { package : string; type_name : string; constructors : string list }
  type func = { package : string; function_name : string }
  type t = Ty of ty | Func of func | File of string

  let lines_of_t (x : t) =
    let ( @ ) pack name = "\"" ^ pack ^ "." ^ name ^ "\"" in
    let s = Printf.sprintf in
    match x with
    | Ty x ->
        List.concat
          [
            [
              s "Derive (Show, Arbitrary) for %s." x.type_name;
              s "Extract Inductive %s => %s" x.type_name
                (x.package @ x.type_name);
              s "  [";
            ];
            Core.List.map
              ~f:(fun c -> s "    %s" (x.package @ c))
              x.constructors;
            [ s "  ]." ];
          ]
    | Func x ->
        [
          s "Extract Constant %s => %s." x.function_name
            (x.package @ x.function_name);
        ]
    | File x -> [ s "QCInclude \"%s\"." x ]
end

type t = {
  extractions : Extractions.t list;
  libraries : string list;
  ml_file : string;
  temp_dir : string;
  target: string;
}
