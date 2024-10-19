open Util
open Config

let name = "compiler_jikken"

let print_version () =
  Printf.printf "%s %s\n" name Const.version

let options =
  ["-f", Arg.Set Env.force, "";
   "-e", Arg.Clear Env.jp, "";
   "--build", Arg.Set_string Env.build, {|<command>  Use <command> to build min-caml instead of "./to_x86 && make"|};
   "-b", Arg.Set_string Env.build, " The same as --build";
   "--compiler-path", Arg.Set_string Env.compiler, {|<path-to-compiler>  Use <path-to-compiler> to run the compiler instead of "min-caml"|};
   "-c", Arg.Set_string Env.compiler, " The same as --compiler-path";
   "--toi", Arg.Int (fun id -> Env.toi_ids := IntSet.add id !Env.toi_ids), " Specify the toi ID to check";
   "--skip-report-check", Arg.Set Env.skip_report_check, " Skip report check";
   "--no-archive", Arg.Set Env.no_archive, " Do not generate archive";
   "--use-cwd", Arg.Set Env.use_cwd, " Use the current working directory to check the compiler";
   "--ci", Arg.Unit (fun () ->
      Env.skip_report_check := true;
      Env.no_archive := true;
      Env.use_cwd := true;
    ), " Run in CI mode";
   "-v", Arg.Unit (fun () -> print_version (); exit 0), " Output the version";
   "--silent", Arg.Unit (fun () -> Config.Log.mode := Silent), "";
   "--verbose", Arg.Unit (fun () -> Config.Log.mode := Verbose), "";
   "--debug", Arg.Unit (fun () -> Config.Log.mode := Debug), ""]

let set_file arg =
  match String.split_on_char '-' arg with
  | [s1;s2] when String.is_int_string s1 && String.length s2 = 6 ->
      Env.no := int_of_string s1;
      Env.id := s2
  | _ ->
    match int_of_string_opt arg with
    | Some no ->
      if !Env.no <> 0 then
        raise @@ Arg.Bad "Multiple week number specified";
      if no < 1 || no > 13 then
        raise @@ Arg.Bad "Week number must be between 1 and 13";
      Env.no := no
    | _ ->
      Env.files := arg :: !Env.files

let usage = Printf.sprintf "Usage: %s XX-YYYYYY <files>\n   or: %s --ci XX --toi Z" name name

let parse () = Arg.parse (Arg.align options) set_file usage
