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

let toi1 () =
  let dir = !!Dir.archive ^ "/test" in
  FileUtil.mkdir ~parent:true dir;
  let files =
    [{name = "sum"; content = Raw Testcases.sum};
     {name = "fib"; content = Raw Testcases.fib}]
  in
  let check file () =
    let exts = ["parsed"; "normalized"] in
    let check ext () =
      let filename = Printf.sprintf "%s/%s.%s" dir file.name ext in
      if Sys.file_exists filename then
        None
      else
        Some (Output_not_found ("*." ^ ext))
    in
    match run_compiler ~dir file () with
    | None -> map check exts ()
    | Some e -> [e]
  in
  concat_map check files ()

let toi2 () =
  let dir = !!Dir.archive ^ "/test" in
  let files =
    [{name = "sum-e"; content = Raw Testcases.sum_e}, 43;
     {name = "fib-e"; content = Raw Testcases.fib_e}, 2]
  in
  let check (file, line) () =
    match run_compiler ~dir ~error:true file () with
    | None ->
        let file_out = Printf.sprintf "%s/%s.out" dir file.name in
        let file_err = Printf.sprintf "%s/%s.err" dir file.name in
        let out = IO.input_file file_out ^ IO.input_file file_err in
        let targets = [Format.sprintf "line %d" line; Format.sprintf "Line %d" line] in
        let has target =
          match String.find out target with
          | _ -> true
          | exception Not_found -> false
        in
        if List.exists has targets then
          []
        else
          [Incorrect_result]
    | Some e -> [e]
  in
  concat_map check files ()

let toi3 () =
  []

let assignments : t =
  {init;
   items =
     [1, Group, toi1;
      2, Group, toi2;
      3, Group, toi3]}
