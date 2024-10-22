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

(* Must be called after clone_* *)
let infer_build_system kind () =
  let find_shallowest_dir_in dir file =
    match Unix.open_and_read_lines (Printf.sprintf "find %s -name %s" dir file) with
    | [] -> None
    | files ->
        files
        |> List.map (Pair.add_left String.length)
        |> List.min
        |> snd
        |> Filename.dirname
        |> Option.some
  in
  let update_param param with_value =
    let ref = compiler_param_of param kind in
    if !ref = param.init then ref := with_value in

  let base_dir = string_of_kind kind in
  let open Option in
  (* hint file,  build command,      compiler exe file path,    exec command,                             input mode *)
  ["dune",       "dune build",       "_build/default/main.exe", "dune exec mincaml --",                   MinCaml;
   "to_x86",     "./to_x86 && make", "min-caml",                "./min-caml",                             MinCaml;
   "Cargo.toml", "cargo build",      "",                        "cargo run -- -i ./tests/pervasives.mli", Explicit]
  |> List.find_map_default (fun (hint, build, compiler_path, exec, arg_style) ->
       let* dir = find_shallowest_dir_in base_dir hint in
       Log.debug "found the submission using %s@." hint;
       compiler_param_of Config.Dir.compiler kind := dir;
       update_param Env.build build;
       update_param Env.compiler_path compiler_path;
       update_param Env.exec exec;
       update_param Env.arg_style arg_style;
       return None) (Some (Compiler_directory_not_found kind))

let build kind () =
  Log.normal "Building the %s compiler...@.@." (string_of_kind kind);
  let dir = !(compiler_param_of Config.Dir.compiler kind) in
  if 0 = Command.run ~filename:"build" "cd %s; %s" dir !(compiler_param_of Env.build kind) then
    None
  else
    (Command.mv [dir^"/build.err"; dir^"/build.out"] Dir.orig_working;
     Some Build_failed)

let check_exists file () =
  if Sys.file_exists file then
    None
  else
    Some (File_not_found file)

let check_compiler_exists kind () =
  let compiler_path = !(compiler_param_of Env.compiler_path kind) in
  if compiler_path = "" then
    None
  else
    check_exists (Printf.sprintf "%s/%s" (string_of_kind kind) compiler_path) ()

let run_compiler ?dir ?(error=false) ?(output=[]) kind {name; content} () =
  let filename =
    match dir with
    | None -> name
    | Some dir -> dir ^ "/"  ^ name
  in
  Log.debug "[Check.run_compiler] filename: %s@." filename;
  if Sys.file_exists filename then invalid_arg "%s" __FUNCTION__;
  let real_filename = filename ^ ".ml" in
  let cout = open_out_bin real_filename in
  let content =
    match content with
    | File input -> IO.CPS.open_in input BatPervasives.input_all
    | Raw s -> s
  in
  output_string cout content;
  close_out cout;
  let path_of =
    let base = Sys.getcwd () in
    fun filename -> base ^ "/" ^ filename in
  let filename = path_of filename in
  let arg = match !(compiler_param_of Env.arg_style kind) with
    | MinCaml -> filename
    | Explicit -> Format.sprintf "-i %s -o %s.s" (path_of real_filename) filename in
  let r = Command.run ~filename "cd %s; %s %s" !(compiler_param_of Config.Dir.compiler kind) !(compiler_param_of Env.exec kind) arg in
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
