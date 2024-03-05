# bugs

Welcome to the bugs found when trying to auto-quickchick OCaml code. The target OCaml file is `prop.ml`. The generated Coq code is `_temp_quickchick_ocaml.v`.

I've labeled a few of the lines in the Coq file to make it easier to manually go between the different bugs.

#### Working Version
- Line 1 reads `| B : nat -> bar.`
- Line 2 reads `  | B 2 => false`
- Line 3 (in the OCaml file) reads `  | B 2 -> false`

QuickChick works fully and finds the edge case.

#### Bug 1
This version compiles and runs, but does not find the bug.
- Line 2 reads `  | B 50 => false`
- Line 3 reads `  | B 50 -> false`

#### Bug 2
This version does not compile, both through line-by-line stepping and via `coqc` on the terminal. My error message is in `images/bug2.png`.

- Line 1 reads `| B : Z -> bar.`

#### Bug 3
This version does not compile, both through line-by-line stepping and via `coqc` on the terminal.

- Line 1 reads `| B : int -> bar.`
- Uncomment the imports for the `CoqOfOCaml` imports in the Coq file.

Importing `int` from `CoqOfOCaml` causes the error message in `images/bug3.png`. I'm assuming we need to import `int` from somewhere else but I haven't been able to find where :(

