open Util
open Assignment
open Check

let init () = []

let kind = Group

let toi2 () =
  exec
    [checkout_repo kind;
     infer_build_system kind;
     build kind;
     check_compiler_exists kind]
  |> Option.to_list

let assignments : t =
  {init;
   check_commit_files = false;
   items =
     [2, kind, toi2]}
