open Util
open Config
open Assignment
open Check

let init () =
  exec
    [change_directory Dir.tmp;
     clone Group;
     find_compiler_directory Group;
     build Group;
     check_compiler_exists Group]
  |> Option.to_list

let output_of file =
  let name = file.name in
  [Format.sprintf "test/%s.out" name;
   Format.sprintf "test/%s.err" name]


let toi23 name files () =
  let exts = ["before_"^name; "after_"^name] in
  let dir = !!Dir.archive ^ "/test" in
  FileUtil.mkdir ~parent:true dir;
  let check file () =
    let output = output_of file in
    let filename_of ext = Printf.sprintf "%s/%s.%s" dir file.name ext in
    let check ext () =
      let filename = filename_of ext in
      if Sys.file_exists filename then
        None
      else
        Some (Output_not_found ("*." ^ ext))
    in
    match run_compiler ~dir ~output file () with
    | None ->
        begin match map check exts () with
        | [] ->
            let files = List.map filename_of exts in
            begin match List.map IO.(CPS.open_in -$- input_all) files with
            | [s1; s2] ->
                if s1 <> s2 then
                  []
                else
                  [Incorrect_result files]
            | _ -> assert false
            end
        | es -> es
        end
    | Some e -> [e]
  in
  concat_map check files ()

let toi2 = toi23 "flatten" [{name = "tuple"; content = Raw Testcases.tuple}]

let toi3 = toi23 "TACE" [{name = "toi3"; content = Raw Testcases.toi3}]

let assignments : t =
  {init;
   check_commit_files = true;
   items =
     [2, Group, toi2;
      3, Group, toi3]}
