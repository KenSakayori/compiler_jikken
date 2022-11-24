open Util
open Config
open Assignment
open Check

let init () = []

let toi2 () =
  exec
    [check_exists_commit_file Group;
     change_directory Dir.tmp;
     clone Group;
     find_compiler_directory Group;
     build Group;
     check_compiler_exists Group]
  |> Option.to_list

let assignments : t =
  {init;
   check_commit_files = false;
   items =
     [2, Group, toi2]}
