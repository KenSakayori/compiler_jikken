open Util
open Config
open Assignment

let command ?filename cmd =
  Log.debug "[Check.command] cwd: %s@." !!Sys.getcwd;
  Format.ksprintf (fun cmd ->
    Log.debug "[Check.command] cmd: %s@." cmd;
    let cmd' =
      match filename with
      | None -> Format.sprintf "%s > /dev/null 2> /dev/null" cmd
      | Some file -> Format.sprintf "%s > %s.out 2> %s.err" cmd file file
    in
    Log.debug "[Check.command] cmd': %s@." cmd';
    let r = Sys.command cmd' in
    Log.debug "[Check.command] r: %d@." r;
    r) cmd

let rec exec acc fs =
  match acc, fs with
  | Some e, _ -> Some e
  | None, [] -> None
  | None, f::fs' -> exec (f()) fs'
let exec fs = exec None fs

let catch f () =
  try
    f ();
    None
  with e -> Some (Exception e)

let change_directory dir =
  catch (fun () -> Sys.chdir dir)

let check_hash hash =
  let hex c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f' in
  String.length hash <= 40 && String.for_all hex hash

let read_commit_file kind =
  let base = commit_file_of_kind kind in
  let file = Printf.sprintf "%s/%s" !!Dir.archive base in
  match IO.CPS.open_in file IO.input_lines with
  | url::hash::_ when check_hash hash -> Ok (url, hash)
  | _ -> Error (Invalid_format base)

let clone kind () =
  let name = string_of_kind kind in
  match read_commit_file kind with
  | Error e -> Some e
  | Ok (url, hash) ->
      Log.normal "Cloning the %s repository...@.@." name;
      if 0 = command "git clone %s %s" url name then
          if 0 = command "cd %s; git checkout %s" name hash then
            None
          else
            Some (Invalid_hash(url,hash))
      else
        Some Clone_failed

(* Must be called after clone_* *)
let find_compiler_directory kind () =
  let name = string_of_kind kind in
  match Unix.open_and_read_lines (Printf.sprintf "find %s -name to_x86" name) with
  | [] -> Some (Compiler_directory_not_found kind)
  | files ->
      let file =
        files
        |> List.map (Pair.add_left String.length)
        |> List.min
        |> snd
        |> Filename.dirname
      in
      compiler_dir_of_kind kind := file;
      None

let build kind () =
  Log.normal "Building the %s compiler...@.@." (string_of_kind kind);
  let dir = !(compiler_dir_of_kind kind) in
  if 0 = command ~filename:"build" "cd %s; %s" dir !Env.build then
    None
  else
    (ignore @@ command "mv %s/build.err %s/build.out %s" dir dir Dir.orig_working;
     Some Build_failed)

let check_exists file () =
  if Sys.file_exists file then
    None
  else
    Some (File_not_found file)

let check_compiler_exists kind =
  check_exists @@ Printf.sprintf "%s/%s" (string_of_kind kind) !Env.compiler

let run_compiler ?dir ?(error=false) ?(output=[]) {name; content} () =
  let filename =
    match dir with
    | None -> name
    | Some dir -> dir ^ "/"  ^ name
  in
  Log.debug "[Check.run_compiler] filename: %s@." filename;
  if Sys.file_exists filename then
    invalid_arg "%s" __FUNCTION__
  else
    let cout = open_out_bin (filename ^ ".ml") in
    let content =
      match content with
      | File input -> IO.CPS.open_in input BatPervasives.input_all
      | Raw s -> s
    in
    output_string cout content;
    close_out cout;
    let r = command ~filename "%s/%s %s" !Dir.group_compiler !Env.compiler filename in
    if 0 = r || error then
      None
    else
      Some (Test_failed output)

let for_all f xs () =
  List.map f xs
  |> exec

let map f xs () =
  List.map f xs
  |> List.filter_map (fun f -> f ())

let concat_map f xs () =
  List.map f xs
  |> List.concat_map (fun f -> f ())


let assignment {init; items} : (int * error list) list =
  match init() with
  | [] ->
      (0, []) ::
      List.L.map items ~f:(fun (n, _, f) ->
        Log.verbose {|Checking %s...@.|} @@ subject_of n;
        n, f ())
  | e -> [0, e]
