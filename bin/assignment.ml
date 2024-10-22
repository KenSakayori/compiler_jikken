open! Util
open Config

type t =
  {init : check;
   check_commit_files : bool;
   items : item list}
and kind = Group | Individual
and item = int * kind * check
and check = unit -> error list

and error =
  | Cannot_extract
  | File_name_invalid of string
  | Directory_not_found of string
  | File_not_found of string
  | Incorrect_result of string list
  | Uncaught_exception
  | Object_file_found of string
  | Build_failed
  | Test_failed of string list
  | Unsupported_week_no of int
  | Unknown_error of string
  | Clone_failed
  | Invalid_input
  | Invalid_format of string
  | Nothing_to_check
  | Compiler_directory_not_found of kind
  | Invalid_hash of string * string
  | Output_not_found of string
  | Exception of exn
  | Command_not_found of string
  | Zip_failed

type testcase = {name: string; content: content}
and content =
  | File of string
  | Raw of string

let string_of_kind = function
  | Group -> "group"
  | Individual -> "individual"

let commit_file_of_kind = function
  | Group -> "group_COMMIT"
  | Individual -> "my_COMMIT"

let compiler_param_of param = function
  | Group -> param.group
  | Individual -> param.individual

let subject_of n =
  if n <= 0 then
    if !Env.jp then "基本チェック" else "Basic check"
  else
    if !Env.jp then
      "問" ^ string_of_int n
    else
      "Toi " ^ string_of_int n

let note_of files =
  if files = [] then
    ""
  else if !Env.jp then
    Format.sprintf "（出力されたzipファイル中の %s を参照してください）" (String.concat ", " files)
  else
    Format.sprintf " (See %s in the generated zip file)" (String.concat ", " files)

let message_of r =
  match r, !Env.jp with
  | Cannot_extract, true -> Printf.sprintf "入力ファイルの展開に失敗しました"
  | Cannot_extract, false -> Printf.sprintf "Cannot extract the input file"
  | File_name_invalid f, true -> Printf.sprintf "ファイル名 %sが不正です" f
  | File_name_invalid f, false -> Printf.sprintf "Filename %s invalid" f
  | Directory_not_found f, true -> Printf.sprintf "ディレクトリ %s が見つかりません" f
  | Directory_not_found f, false -> Printf.sprintf "Directory %s not found" f
  | File_not_found f, true -> Printf.sprintf "ファイル %s が見つかりません" f
  | File_not_found f, false -> Printf.sprintf "File %s not found" f
  | Incorrect_result files, true -> Printf.sprintf "結果が正しくありません%s" (note_of files)
  | Incorrect_result files, false -> Printf.sprintf "Incorrect result%s" (note_of files)
  | Uncaught_exception, true -> Printf.sprintf "例外が発生しました"
  | Uncaught_exception, false -> Printf.sprintf "Uncaught exception occurred"
  | Object_file_found s, true -> Printf.sprintf "%s を消してください" s
  | Object_file_found s, false -> Printf.sprintf "Remove %s" s
  | Build_failed, true -> Printf.sprintf "ビルドに失敗しました（build.out および build.err を参照してください）"
  | Build_failed, false -> Printf.sprintf "Build failed (See build.out and build.err)"
  | Test_failed files, true -> Printf.sprintf "テストに失敗しました%s" (note_of files)
  | Test_failed files, false -> Printf.sprintf "Test failed%s" (note_of files)
  | Unsupported_week_no n, true -> Printf.sprintf "第%d週の課題はサポートしていません" n
  | Unsupported_week_no n, false -> Printf.sprintf "Not support: Week %d" n
  | Unknown_error s, true -> Printf.sprintf "不明なエラー (%s)" s
  | Unknown_error s, false -> Printf.sprintf "Unknown error (%s)" s
  | Clone_failed, true -> Printf.sprintf "リポジトリのクローンに失敗しました"
  | Clone_failed, false -> Printf.sprintf "Failed to clone the repository"
  | Exception e, true -> Printf.sprintf "エラー (%s)" (Printexc.to_string e)
  | Exception e, false -> Printf.sprintf "Error (%s)" (Printexc.to_string e)
  | Invalid_input, _ -> Printf.sprintf "%s" Command_line.usage
  | Nothing_to_check, true -> Printf.sprintf "実行するテストがありません（コマンドの入力を確認してください）"
  | Nothing_to_check, false -> Printf.sprintf "No test to run (Check the command input)"
  | Compiler_directory_not_found _, true -> Printf.sprintf "リポジトリにコンパイラのディレクトリが見つかりません"
  | Compiler_directory_not_found _, false -> Printf.sprintf "Cannot find the directory of a compiler in the repository"
  | Invalid_format s, true -> Printf.sprintf "%s のフォーマットが不正です" s
  | Invalid_format s, false -> Printf.sprintf "Invalid format %s" s
  | Invalid_hash(url,hash), true -> Printf.sprintf "コミット %s が %s に見つかりません" hash url
  | Invalid_hash(url,hash), false -> Printf.sprintf "Cannot find commit <%s> in %s" hash url
  | Output_not_found s, true -> Printf.sprintf "出力ファイル %s が見つかりません" s
  | Output_not_found s, false -> Printf.sprintf "Output files %s not found" s
  | Command_not_found f, true -> Printf.sprintf "コマンド %s が見つかりません" f
  | Command_not_found f, false -> Printf.sprintf "Command %s not found" f
  | Zip_failed, true -> Printf.sprintf "zip コマンドが失敗しました (zip.err を参照してください)"
  | Zip_failed, false -> Printf.sprintf "Command zip failed (See zip.err)"
