let compile file =
  try
    let g = Codegen.new_generator file in
    let s = Stream.open_stream file in
    let o = Filename.chop_extension file in
    parse s g;
    Stream.close_stream s;
    Codegen.close_generator g;
    let _ = Sys.command ("nasm -f macho " ^ g.file) in
    let _ = Sys.command ("gcc -o " ^out ^ " " ^ out ^ ".o") in
    ()
  with
    | Syntax_error e ->
      printf "syntax error: %s\n" e;
    | Sys_error _ ->
      print_string "no file found\n"

let help name = printf "%s <file>\n" name

let () =
  if Array.length Sys.argv = 1
    then help (Array.get Sys.argv 0)
    else
      let file = Array.get Sys.argv 1 in
        printf "compiling %s\n" file
        compile file
