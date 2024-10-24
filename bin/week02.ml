open Util
open Config
open Assignment
open Check

let init () =
  exec
    [checkout_repo Group;
     infer_build_system Group;
     build Group;
     check_compiler_exists Group]
  |> Option.to_list

let output_of file =
  let name = file.name in
  [Format.sprintf "test/%s.out" name;
   Format.sprintf "test/%s.err" name]

let toi2 () =
  let dir = !!Dir.archive ^ "/test" in
  let dir = if !Env.no_clone then Dir.tmp ^ "/" ^ dir else dir in
  FileUtil.mkdir ~parent:true dir;
  let files =
    [{name = "sum"; content = Raw Testcases.sum};
     {name = "fib"; content = Raw Testcases.fib}]
  in
  let check file () =
    let output = output_of file in
    let exts = ["before_CSE"; "after_CSE"] in
    let check ext () =
      let filename = Printf.sprintf "%s/%s.%s" dir file.name ext in
      if Sys.file_exists filename then
        None
      else
        Some (Output_not_found ("*." ^ ext))
    in
    match run_compiler ~dir ~output Group file () with
    | None -> map check exts ()
    | Some e -> [e]
  in
  concat_map check files ()

let assignments : t =
  {init;
   check_commit_files = true;
   items =
     [2, Group, toi2]}
