open Util
open Config
open Assignment

let assiginments : (int * Assignment.t) list =
  [ 1, Week01.assignments]

let make_archive_dir () =
  let dir = Printf.sprintf "%s/%s" Dir.tmp !!Dir.archive in
  FileUtil.mkdir ~parent:true dir

let make_env_file () =
  make_archive_dir ();
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

let show_error_and_exit es =
  List.iter (Printf.printf "%s\n" -| message_of) es;
  finalize ();
  exit 1

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

let check_exists_commit_file kind =
  let file = commit_file_of_kind kind in
  match find_file file with
  | None -> Some (File_not_found file)
  | Some file' ->
      make_archive_dir ();
      FileUtil.cp [file'] (Printf.sprintf "%s/%s/%s" Dir.tmp !!Dir.archive file);
      None

let get_kinds () =
  !!get_assignment.items
  |> List.map Triple.snd

let get_commit_files () =
  !!get_kinds
  |> List.unique
  |> List.map commit_file_of_kind

let check_arg () =
  if !Env.no = 0 then
    [Invalid_input]
  else
    match check_exists_report () with
    | Some e -> [e]
    | None ->
        !!get_kinds
        |> List.unique
        |> List.filter_map check_exists_commit_file

let make_archive () =
  let filename = Printf.sprintf "%02d-%s.zip" !Env.no !Env.id in
  Log.normal "Generating %s...@." filename;
  Sys.chdir Dir.orig_working;
  Sys.chdir Dir.tmp;
  let r = Check.command "zip -r %s %s" filename !!Dir.archive in
  assert (0 = r);
  ignore @@ Check.command "mv %s %s" filename Dir.orig_working

let main () =
  let@ () = Fun.protect ~finally:finalize in
  init();
  begin match check_arg () with
  | [] -> ()
  | es -> show_error_and_exit es
  end;
  !!get_assignment
  |> Check.assignment
  |> List.iter show_results;
  make_archive ()

let () = if not !Sys.interactive then main()
