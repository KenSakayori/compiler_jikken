open Util
open Config
open Assignment
open Check

let init () = []

let toi2 () =
  exec
    [change_directory Dir.tmp;
     clone Individual;
     find_compiler_directory Individual;
     build Individual;
     check_compiler_exists Individual]
  |> Option.to_list

let assignments : t =
  {init;
   items =
     [2, Individual, toi2]}
