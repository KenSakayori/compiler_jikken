open Util

module Const = struct
  let version = "0.1.0"
  let report_name = "report"
  let report_exts = ["txt"; "md"; "pdf"]
end

module Env = struct
  let no = ref 0
  let id = ref ""
  let force = ref false
  let jp = ref true
  let files : string list ref = ref []
  let build = ref "./to_x86 && make"
  let compiler = ref "min-caml"
  let report_file = ref ""
end

module Dir = struct
  let orig_working = Sys.getcwd()
  let tmp = "_comp_tmp_" ^ Unix.string_of_time()
  let group_compiler = ref ""
  let individual_compiler = ref ""
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
