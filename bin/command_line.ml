open Util
open Config

let name = "compiler_jikken"

let print_version () =
  Printf.printf "%s %s\n" name Const.version

module MyArgs = struct
  let default_build = ref None
  let group_build = ref None
  let individual_build = ref None
  let default_compiler_path = ref None
  let group_compiler_path = ref None
  let individual_compiler_path = ref None
  let default_exec = ref None
  let group_exec = ref None
  let individual_exec = ref None
end

let arg_set_opt_string ref = Arg.String (fun str -> ref := Some str)

let options =
  ["-f", Arg.Set Env.force, "";
   "-e", Arg.Clear Env.jp, "";
   "--build",                    arg_set_opt_string MyArgs.default_build,            "<command>  Use <command> to build compiler without inferring";
   "--build-group",              arg_set_opt_string MyArgs.group_build,              "<command>  Similar to --build, but for group compiler";
   "--build-individual",         arg_set_opt_string MyArgs.individual_build,         "<command>  Similar to --build, but for individual compiler";
   "-b",                         arg_set_opt_string MyArgs.default_build,            " The same as --build";
   "--compiler-path",            arg_set_opt_string MyArgs.default_compiler_path,    "<path>  Check if the compiler exist in <path> after build";
   "--compiler-path-group",      arg_set_opt_string MyArgs.group_compiler_path,      "<path>  Similar to --compiler-path, but for group compiler";
   "--compiler-path-individual", arg_set_opt_string MyArgs.individual_compiler_path, "<path>  Similar to --compiler-path, but for individual compiler";
   "-c",                         arg_set_opt_string MyArgs.default_compiler_path,    " The same as --compiler-path";
   "--exec",                     arg_set_opt_string MyArgs.default_exec,             "<command>  Use <command> to run the compiler without inferring";
   "--exec-group",               arg_set_opt_string MyArgs.group_exec,               "<command>  Similar to --exec, but for group compiler";
   "--exec-individual",          arg_set_opt_string MyArgs.individual_exec,          "<command>  Similar to --exec, but for individual compiler";
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

let parse () =
  Arg.parse (Arg.align options) set_file usage;
  let (||) x y =
    match x with
    | None -> y
    | Some _ -> x in
  let override_env ref option = Option.iter (fun value -> ref := value) option in
  override_env Env.group_build (!MyArgs.group_build || !MyArgs.default_build);
  override_env Env.individual_build (!MyArgs.individual_build || !MyArgs.default_build);
  override_env Env.group_compiler_path (!MyArgs.group_compiler_path || !MyArgs.default_compiler_path);
  override_env Env.individual_compiler_path (!MyArgs.individual_compiler_path || !MyArgs.default_compiler_path);
  override_env Env.group_exec (!MyArgs.group_exec || !MyArgs.default_exec);
  override_env Env.individual_exec (!MyArgs.individual_exec || !MyArgs.default_exec);
