open Util
open Assignment
open Check

let init () = []

let toi2 () =
  exec
    [checkout_repo Individual;
     infer_build_system Individual;
     build Individual;
     check_compiler_exists Individual]
  |> Option.to_list

let assignments : t =
  {init;
   check_commit_files = false;
   items =
     [2, Individual, toi2]}
