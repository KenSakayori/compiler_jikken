open Util
open Config
open Assignment

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
      if 0 = Command.run "git clone %s %s" url name then
          if 0 = Command.run "cd %s; git checkout %s" name hash then
            None
          else
            Some (Invalid_hash(url,hash))
      else
        Some Clone_failed

let checkout_repo kind () =
  exec
    [change_directory Dir.tmp;
     clone kind]

type check_fn = unit -> error option

let cancel : check_fn -> check_fn = Fun.const2 None

(* Must be called after checkout_repo *)
let find_compiler_directory kind () =
  if !Env.use_cwd then
    (compiler_dir_of_kind kind := ".";
    None)
  else
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
  if 0 = Command.run ~filename:"build" "cd %s; %s" dir !Env.build then
    None
  else
    (Command.mv [dir^"/build.err"; dir^"/build.out"] Dir.orig_working;
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
    let r = Command.run ~filename "%s/%s %s" !Dir.group_compiler !Env.compiler filename in
    if 0 = r || error then
      None
    else
      Some (Test_failed output)

let find_file filename =
  !Env.files @ Array.to_list @@ Sys.readdir "."
  |> List.find_opt (Filename.basename |- (=) filename)

let check_exists_report () =
  let files =
    Const.report_exts
    |> List.map (Printf.sprintf "%s.%s" Const.report_name)
    |> List.filter_map find_file
  in
  match files with
  | [] -> Some (File_not_found (Printf.sprintf "%s.{%s}" Const.report_name (String.join "|" Const.report_exts)))
  | file::_ ->
      Env.report_file := file;
      FileUtil.cp [file] (Printf.sprintf "%s/%s/%s" Dir.tmp !!Dir.archive file);
      None

let check_exists_commit_file kind () =
  let file = commit_file_of_kind kind in
  match find_file file with
  | None -> Some (File_not_found file)
  | Some file' ->
      Command.make_archive_dir ();
      FileUtil.cp [file'] (Printf.sprintf "%s/%s/%s" Dir.tmp !!Dir.archive file);
      None

let for_all f xs () =
  List.map f xs
  |> exec

let map f xs () =
  List.map f xs
  |> List.filter_map (fun f -> f ())

let concat_map f xs () =
  List.map f xs
  |> List.concat_map (fun f -> f ())


let assignment {init; items; _} : (int * error list) list =
  match init() with
  | [] ->
      (0, []) ::
      List.L.map items ~f:(fun (n, _, f) ->
        Log.verbose {|Checking %s...@.|} @@ subject_of n;
        n, f ())
  | e -> [0, e]

let exists_report = check_exists_report
let exists_commit_file = check_exists_commit_file
