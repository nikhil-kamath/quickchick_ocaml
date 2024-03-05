type bar =
  | A
  | B of int

type bar2 =
  | X
  | Y of bar2

let foo (l : bar) : bool =
  match l with
  | A -> true
  | B 2 -> false (* line 3 *)
  | _ -> true



