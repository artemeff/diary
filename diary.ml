let usage = "usage: " ^ Sys.argv.(0) ^ " [-l number] [-a string]"
let file  = "/.diary"

(* get users diary path *)
let diary_path =
  try (Sys.getenv "HOME") ^ file
  with Not_found -> failwith "Fatal Error"

let format_diary_log str =
  str ^ "\n"

let timestamp =
  int_of_float (Unix.time())

let compose_log message =
  (string_of_int timestamp) ^ ":" ^ message ^ "\n"

let compose_message_from argv =
  (String.concat " " (Array.to_list (Array.sub argv 1 ((Array.length argv) - 1))))

(* walking in file *)
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let walk_file path f acc =
  let ic = open_in path in
  let rec aux f acc inc =
    match input_line_opt ic with
    | Some line ->
        aux f (f inc line acc) (inc + 1)
    | None ->
        close_in ic;
        acc
  in
  aux f acc 0

(* count lines in file *)
let count_file_lines =
  walk_file diary_path (fun line_num line acc -> acc + 1) 0

(* read last n file lines *)
let read_file_lines n =
  let walk_file_fun line_num line acc =
    if line_num >= n
    then acc ^ (format_diary_log line)
    else acc
  in
  walk_file diary_path walk_file_fun ""

(* show log from n last lines from diary file *)
let read_log number_of_logs =
  let total = count_file_lines in
  let log = read_file_lines (total - number_of_logs) in
  Printf.printf "%s" log

(* write message to log in format `timestamp:message` *)
let write_log message =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 diary_path in
  output_string oc (compose_log message);
  close_out oc

let speclist = [
  ("-l", Arg.Int (read_log), "Show diary log");
  ("-", Arg.String (write_log), "Add log to diary");
]

let badarg x =
  raise (Arg.Bad ("Bad argument:" ^ x))

let () =
  match Sys.argv.(1) with
  | "-l" -> read_log (int_of_string Sys.argv.(2))
  | _ -> write_log (compose_message_from Sys.argv)
  (* Arg.parse speclist badarg usage *)
  (* Printf.printf "%s" (String.concat "\n" (Array.to_list Sys.argv)) *)
