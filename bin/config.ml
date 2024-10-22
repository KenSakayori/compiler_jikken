open Util

module Const = struct
  let version = "v2024.1"
  let report_name = "report"
  let report_exts = ["txt"; "md"; "pdf"]
end

type 't compiler_param = {
  init: 't;
  group: 't ref;
  individual: 't ref;
}
let init_compiler_param init = {
  init;
  group = ref init;
  individual = ref init;
}

module ArgStyle = struct
  type t = MinCaml | Explicit

  let arg_style_of_string = function
    | "mincaml" (* supports typo *)
    | "min-caml" -> MinCaml
    | "explicit" -> Explicit
    | _ -> invalid_arg "%s" __FUNCTION__
end

module Env = struct
  let no = ref 0
  let id = ref ""
  let force = ref false
  let jp = ref true
  let files : string list ref = ref []
  let report_file = ref ""

  let build = init_compiler_param ""
  let compiler_path = init_compiler_param ""
  let exec = init_compiler_param ""
  let arg_style = init_compiler_param ArgStyle.MinCaml
end

module Dir = struct
  let orig_working = Sys.getcwd()
  let tmp = "_comp_tmp_" ^ Unix.string_of_time()
  let compiler = init_compiler_param ""
  let archive() = Printf.sprintf "%02d-%s" !Env.no !Env.id
end

module Log = struct
  type mode = Silent | Normal | Verbose | Debug

  let mode = ref Normal
  let log m f =
    if m <= !mode then Format.printf f else Format.ifprintf Format.std_formatter f
  let normal f = log Normal f
  let debug f = log Debug f
  let verbose f = log Verbose f
end
