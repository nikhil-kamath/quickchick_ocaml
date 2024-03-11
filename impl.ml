type tree =
| E
| T of tree * int * int * tree
[@@deriving sexp, quickcheck]


let fuel : int = 10000


let rec insert (k: int) (v: int) (t: tree) =
  match t with
  | E -> T (E, k, v, E)
  | T (l, k', v', r) ->
    (*! *)
      (* if k < k' then T ((insert k v l), k', v', r)
      else if k' < k then T (l, k', v', (insert k v r))
      else T (l, k', v, r) *)
    (*!! insert_1 *)
      (* ! *)
      let _ = ignore (l, k', v', r, insert) in T (E, k, v, E)

    (*!! insert_2 *)
      (*!
      if k < k' then T ((insert k v l), k', v', r)
      else T (l, k', v, r)
      *)
    (*!! insert_3 *)
      (*!
      if k < k' then T ((insert k v l), k', v', r)
      else if k' < k then T (l, k', v', (insert k v r))
      else T (l, k', v', r)
      *)

let rec join (l: tree) (r: tree) =
  match l, r with
  | E, _ -> r
  | _, E -> l
  | T (l, k, v, r), T (l', k', v', r') ->
    T (l, k, v, T ((join r l'), k', v', r'))

let rec delete (k: int) (t: tree) =
  match t with
  | E -> E
  | T (l, k', v', r) ->
  (*! *)
  if k < k' then T ((delete k l), k', v', r)
  else if k' < k then T (l, k', v', (delete k r))
  else join l r
  (*!! delete_4 *)
  (*!
  let _ = ignore v' in
  if k < k' then delete k l
  else if k' < k then delete k r
  else join l r
  *)
  (*!! delete_5 *)
  (*!
  if k' < k then T ((delete k l), k', v', r)
  else if k < k' then T (l, k', v', (delete k r))
  else join l r
  *)


let rec below (k: int) (t: tree) =
  match k, t with
  | _, E -> E
  | k, T (l, k', v, r) ->
    if k <= k' then below k l
    else T (l, k', v, below k r)

let rec above (k: int) (t: tree) =
  match k, t with
  | _, E -> E
  | k, T (l, k', v, r) ->
    if k' <= k then above k r
    else T (above k l, k', v, r)

let rec union_ (l: tree) (r: tree) (f: int) =
  match f with
  | 0 -> E
  | _ -> let f' = f-1 in
    match l, r with
    | E, _ -> r
    | _, E -> l
    (*! *)
    | T (l, k, v, r), t ->
      T (union_ l (below k t) f', k, v, union_ r (above k t) f')
    (*!! union_6 *)
    (*!
    | T (l, k, v, r), T (l', k', v', r') ->
      T (l, k, v, T (union_ r l' f', k', v', r'))
    *)
    (*!! union_7 *)
    (*!
    | T (l, k, v, r), T (l', k', v', r') ->
      if k == k' then T (union_ l l' f', k, v, union_ r r' f')
      else if k < k' then T (l, k, v, T (union_ r l' f', k', v', r'))
      else union_ (T (l', k', v', r')) (T (l, k, v, r)) f'
    *)
    (*!! union_8 *)
    (*!
    | T (l, k, v, r), T (l', k', v', r') ->
    if k == k'  then T (union_ l l' f', k, v, union_ r r' f')
    else if k < k'   then T (union_ l (below k l') f', k, v,
                            union_ r (T (above k l', k', v', r')) f')
      else union_ (T (l', k', v', r')) (T (l, k, v, r)) f'
    *)

let union (l: tree) (r: tree) =
  union_ l r fuel

let rec find (k: int) (t: tree): int option =
  match k, t with
  | _, E -> None
  | k, T (l, k', v', r) ->
    if k < k' then find k l
    else if k' < k then find k r
    else Some v'

let rec size (t: tree) =
  match t with
  | E -> 0
  | T (l, _, _, r) -> 1 + size l + size r





let prop_InsertPost =
  let rec keys (t : tree) : int list =
  match t with
  | E -> []
  | T (l, k, _v, r) ->
      let lk = keys l in
      let rk = keys r in
      [ k ] @ lk @ rk in
let rec all (f : 'a -> bool) (l : 'a list) : bool =
  match l with [] -> true | x :: xs -> f x && all f xs in
  let rec isBST (t : tree) : bool =
  match t with
  | E -> true
  | T (l, k, _, r) ->
      isBST l && isBST r
      && all (fun k' -> k' < k) (keys l)
      && all (fun k' -> k' > k) (keys r) in
 fun t k k' v -> (not (isBST t)) || (find k' (insert k v t) = if k = k' then Some v else find k' t)