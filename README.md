# Quickchick_ocaml

Test your OCaml code via translation into Coq with the power of Coq inductives and QuickChick. 

Say you want to test this function, defined in `prop.ml`:
```ocaml
type bar =
  | A
  | B of int

let foo (l : bar) : bool =
  match l with
  | B 1 -> false
  | _ -> true
```

QuickChick will find the counterexample, `B 1`. The setup for this is fairly simple:

```ocaml
let () =
  set_path "<path to folder with .ml files>";
  set_file "prop.ml";
  add_type "Prop" "bar" ["A"; "B"];
  set_function "Prop" "foo";
  ()

let main () =
  match quickchick () with
  | Some f -> Core.Printf.printf "Found counterexample in %f seconds\n" f
  | _ -> Core.Printf.printf "Error in Coq translation\n"
```

Currently working on a deriver to automatically generate this setup code, which can be found [here](https://github.com/nikhil-kamath/ppx_quickchick_ocaml).

