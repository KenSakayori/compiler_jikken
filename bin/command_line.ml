open Util
open Config

let name = "compiler_jikken"

let print_version () =
  Printf.printf "%s %s\n" name Const.version

module MyArgs = struct
  type 't option_kind = {
    param: 't compiler_param;
    default: 't ref;
  }
  let init_option param = {
    param;
    default = ref param.init;
  }
  let build = init_option Env.build
  let compiler_path = init_option Env.compiler_path
  let exec = init_option Env.exec
  let arg_style = init_option Env.arg_style
end

let spec_input_style ref = Arg.String (fun s ->
  ref := try ArgStyle.arg_style_of_string s with _ -> raise @@ Arg.Bad {|--arg-style must be either "min-caml" or "explicit"|}
)

let options =
  ["-f", Arg.Set Env.force, "";
   "-e", Arg.Clear Env.jp, "";
   "--build",                    Arg.Set_string MyArgs.build.default,                  "<command>  Use <command> to build compiler without inferring";
   "--build-group",              Arg.Set_string MyArgs.build.param.group,              "<command>  Similar to --build, but for group compiler";
   "--build-individual",         Arg.Set_string MyArgs.build.param.individual,         "<command>  Similar to --build, but for individual compiler";
   "-b",                         Arg.Set_string MyArgs.build.default,                  " The same as --build";
   "--compiler-path",            Arg.Set_string MyArgs.compiler_path.default,          "<path>  Check if the compiler exist in <path> after build";
   "--compiler-path-group",      Arg.Set_string MyArgs.compiler_path.param.group,      "<path>  Similar to --compiler-path, but for group compiler";
   "--compiler-path-individual", Arg.Set_string MyArgs.compiler_path.param.individual, "<path>  Similar to --compiler-path, but for individual compiler";
   "-c",                         Arg.Set_string MyArgs.compiler_path.default,          " The same as --compiler-path";
   "--exec",                     Arg.Set_string MyArgs.exec.default,                   "<command>  Use <command> to run the compiler without inferring";
   "--exec-group",               Arg.Set_string MyArgs.exec.param.group,               "<command>  Similar to --exec, but for group compiler";
   "--exec-individual",          Arg.Set_string MyArgs.exec.param.individual,          "<command>  Similar to --exec, but for individual compiler";
   "--arg-style",                spec_input_style MyArgs.arg_style.default,            "(mincaml|explicit)  Style of command line arguments of the compiler";
   "--arg-style-group",          spec_input_style MyArgs.arg_style.param.group,        "(mincaml|explicit)  Similar to --arg-style, but for group compiler";
   "--arg-style-individual",     spec_input_style MyArgs.arg_style.param.individual,   "(mincaml|explicit)  Similar to --arg-style, but for individual compiler";
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

  let open MyArgs in

  let apply_default opt =
    let to_ref ref = if !ref = opt.param.init then ref := !(opt.default) in
    to_ref opt.param.group;
    to_ref opt.param.individual;
  in

  apply_default build;
  apply_default compiler_path;
  apply_default exec;
  apply_default arg_style;
