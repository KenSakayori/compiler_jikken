open Util
open Config
open Assignment
open Check

let init () =
  exec
    [checkout_repo Group |&!Env.use_cwd&> cancel;
     infer_build_system Group;
     build Group;
     check_compiler_exists Group]
  |> Option.to_list

let output_of file =
  let name = file.name in
  [Format.sprintf "test/%s.out" name;
   Format.sprintf "test/%s.err" name]

let toi4 () =
  let dir = !!Dir.archive ^ "/test" in
  FileUtil.mkdir ~parent:true dir;
  let files =
    [{name = "fun"; content = Raw Testcases.fun_};
     {name = "partial"; content = Raw Testcases.partial}]
  in
  let check file () =
    let output = output_of file in
    match run_compiler ~dir ~output Group file () with
    | None ->
        let filename = Printf.sprintf "%s/%s.s" dir file.name in
        if Sys.file_exists filename then
          []
        else
          [Output_not_found "*.s"]
    | Some e -> [e]
  in
  concat_map check files ()

let assignments : t =
  {init;
   check_commit_files = true;
   items = [4, Group, toi4]}
