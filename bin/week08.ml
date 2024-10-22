open Util
open Config
open Assignment
open Check

let init () = []

let kind = Individual

let toi1 () =
  exec
    [check_exists_commit_file kind;
     change_directory Dir.tmp;
     clone kind;
     infer_build_system kind;
     build kind;
     check_compiler_exists kind]
  |> Option.to_list

let assignments : t =
  {init;
   check_commit_files = false;
   items =
     [1, kind, toi1]}
