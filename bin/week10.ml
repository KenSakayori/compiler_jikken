open Util
open Config
open Assignment
open Check

let init () = []

let kind = Individual

let toi1 () =
  exec
    [checkout_repo kind |&!Env.use_cwd&> cancel;
     find_compiler_directory kind;
     build kind;
     check_compiler_exists kind]
  |> Option.to_list

let assignments : t =
  {init;
   check_commit_files = false;
   items =
     [1, kind, toi1]}
