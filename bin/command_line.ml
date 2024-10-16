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
      Env.files := arg :: !Env.files

let usage = Printf.sprintf "Usage: %s XX-YYYYYY <files>" name

let parse () = Arg.parse (Arg.align options) set_file usage
