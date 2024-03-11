open Core
open Poly

let find_time_elapsed (strings : string list) : float option =
  let starts_with_time_elapsed s = String.is_prefix s ~prefix:"Time Elapsed" in
  let extract_time s =
    match String.split ~on:' ' s with
    | _ :: _ :: time :: _ -> (
        try Some (Float.of_string (String.chop_suffix_exn time ~suffix:"s"))
        with _ -> None)
    | _ -> None
  in
  match List.find ~f:starts_with_time_elapsed strings with
  | Some s -> extract_time s
  | None -> None

let append_to_file (filename : string) (lines : string list) : unit =
  let oc = Out_channel.create ~append:true filename in
  List.iter ~f:(fun line -> Out_channel.output_string oc (line ^ "\n")) lines;
  Out_channel.close oc

let write_to_file (filename : string) (lines : string list) : unit =
  let oc = Out_channel.create filename in
  List.iter ~f:(fun line -> Out_channel.output_string oc (line ^ "\n")) lines;
  Out_channel.close oc

let replace_in_file filename x y =
  let input_channel = In_channel.create filename in
  let lines = In_channel.input_lines input_channel in
  let updated_lines =
    List.map lines ~f:(fun line ->
        line |> String.split ~on:' '
        |> List.map ~f:(fun word -> if String.equal word x then y else word)
        |> String.concat ~sep:" ")
  in
  In_channel.close input_channel;

  let output_channel = Out_channel.create filename in
  List.iter updated_lines ~f:(fun line ->
      Out_channel.output_string output_channel (line ^ "\n"));
  Out_channel.close output_channel

let from_until ~filename ~prefix ~suffix =
  let input_channel = In_channel.create filename in
  let lines = In_channel.input_lines input_channel in
  let rec find_inductive ls =
    match ls with
    | l :: ls ->
        if String.is_prefix l ~prefix then l :: ls else find_inductive ls
    | [] ->
        failwith
          (Printf.sprintf "Could not find prefix `%s`. Error in coq-of-ocaml?"
             prefix)
  in
  let lines = find_inductive lines in
  let rec find_period ls acc =
    match ls with
    | l :: ls ->
        if String.is_suffix l ~suffix then List.rev (l :: acc)
        else find_period ls (l :: acc)
    | [] -> failwith (Printf.sprintf "Could not find suffix `%s`?" suffix)
  in
  find_period lines []

let get_translated_type filename type_name =
  from_until ~filename
    ~prefix:(Printf.sprintf "Inductive %s" type_name)
    ~suffix:"."

let get_translated_function filename function_name =
  from_until ~filename
    ~prefix:(Printf.sprintf "Definition %s" function_name)
    ~suffix:"."

let axiomatize decl =
  let decl = String.substr_replace_all decl ~pattern:":=" ~with_:"." in
  String.substr_replace_all decl ~pattern:"Definition" ~with_:"Axiom"

let get_axiomatized_function filename function_name =
  [
    get_translated_function filename function_name |> List.hd_exn |> axiomatize;
  ]
