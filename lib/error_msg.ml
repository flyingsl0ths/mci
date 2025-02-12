let any_errors = ref false
let file_name = ref ""
let line_num = ref 1
let line_pos = ref [ 1 ]
let source_stream = ref stdin

exception Error

let reset () =
  any_errors := false;
  file_name := "";
  line_num := 1;
  line_pos := [ 1 ];
  source_stream := stdin

let rec look pos (xs, n) =
  match xs with
  | [] -> Printf.printf ":%d" n
  | x :: xs ->
      if x < pos then Printf.printf ":%d.%d" n (pos - x)
      else look pos (xs, n - 1)

let error pos msg =
  any_errors := true;
  Printf.printf "%s" !file_name;
  look pos (!line_pos, !line_num);
  Printf.printf ":%s\n" msg

let impossible msg =
  Printf.printf "Error: Compiler bug: %s\n" msg;
  flush stdout;
  raise Error
