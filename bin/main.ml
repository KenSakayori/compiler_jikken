open Util
open Config
open Assignment

let assiginments : (int * Assignment.t) list =
  [ 1, Week01.assignments;
    2, Week02.assignments;
    3, Week03.assignments]

let make_env_file () =
  Command.make_archive_dir ();
  let@ cout = IO.CPS.open_out (Printf.sprintf "%s/%s/ENV" Dir.tmp !!Dir.archive) in
  let write s1 s2 = output_string cout @@ Printf.sprintf "%s: %s\n" s1 s2 in
  write "ver" Const.version;
  write "date" @@ Unix.open_and_read_line "date";
  write "week" (Printf.sprintf "%02d" !Env.no);
  write "ID" !Env.id;
  write "ocaml" Sys.ocaml_version;
  write "system" @@ Unix.open_and_read_line "uname -a";
  write "gcc" @@ Unix.open_and_read_line "gcc --version"

let init () =
  Command_line.parse ();
  Log.debug "Dir.tmp: %s@." Dir.tmp;
  if not @@ Sys.file_exists Dir.tmp then Sys.mkdir Dir.tmp 0o755;
  make_env_file ()

let finalize () =
  Sys.chdir Dir.orig_working;
  if Sys.file_exists Dir.tmp && !Log.mode <> Debug then
    FileUtil.rm ~recurse:true [Dir.tmp]

let exit_with_error () =
  finalize ();
  exit 1

let show_error_and_exit es =
  List.iter (Printf.printf "%s\n" -| message_of) es;
  exit_with_error ()

let get_assignment () =
  let n = !Env.no in
  try
    List.assoc n assiginments
  with Not_found ->
    show_error_and_exit [Unsupported_week_no n]

let show_results (n, errors) =
  Printf.printf "[%s] " (subject_of n);
  match errors with
  | [] -> Printf.printf "OK\n\n"
  | _ ->
      Printf.printf "NG\n";
      errors
      |> List.map message_of
      |> List.unique
      |> List.iter (Printf.printf "- %s\n");
      Printf.printf "\n"

let get_kinds_for_check () =
  let {items; check_commit_files; _} = !!get_assignment in
  if check_commit_files then
    items
    |> List.map Triple.snd
    |> List.unique
  else
    []

let check_zip_command_exists () =
  0 = Command.run "zip -v"

let check () =
  if !Env.no = 0 then
    [Invalid_input]
  else if not (check_zip_command_exists ()) then
    [Command_not_found "zip"]
  else
    match Check.exists_report () with
    | Some e -> [e]
    | None ->
        !!get_kinds_for_check
        |> List.filter_map (Check.exists_commit_file -$- ())

let make_archive () =
  let filename = Printf.sprintf "%02d-%s.zip" !Env.no !Env.id in
  Log.normal "Generating %s...@." filename;
  Sys.chdir Dir.orig_working;
  Sys.chdir Dir.tmp;
  let r = Command.run ~filename:"zip" "zip -r %s %s" filename !!Dir.archive in
  if 0 <> r then
    (Command.mv ["zip.err"] Dir.orig_working;
    Some Zip_failed)
  else
    (Command.mv [filename] Dir.orig_working;
     None)

let main () =
  let@ () = Fun.protect ~finally:finalize in
  init();
  let es = check () in
  if es <> [] then show_error_and_exit es;
  let errors = Check.assignment !!get_assignment in
  if errors <> [] then List.iter show_results errors;
  (match List.assoc_opt 0 errors with
   | Some (_::_) -> exit_with_error ()
   | _ -> ());
  match make_archive () with
  | Some e -> show_error_and_exit [e]
  | _ -> ()

let () = if not !Sys.interactive then main()
