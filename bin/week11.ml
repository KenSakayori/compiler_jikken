open Util
open Config
open Assignment
open Check

let init () = []

let kind = Individual

let toi2 () =
  exec
    [check_exists_commit_file kind;
     change_directory Dir.tmp;
     clone kind]
  |> Option.to_list

let assignments : t =
  {init;
   check_commit_files = false;
   items =
     [2, kind, toi2]}
