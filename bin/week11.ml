open Util
open Assignment
open Check

let init () = []

let kind = Individual

let toi2 () =
  exec
    [checkout_repo kind]
  |> Option.to_list

let assignments : t =
  {init;
   check_commit_files = false;
   items =
     [2, kind, toi2]}
