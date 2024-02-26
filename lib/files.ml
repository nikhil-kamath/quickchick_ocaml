open Core

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

