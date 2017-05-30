let compile file =
  try
    let g = Codegen.new_generator file in
    let s = Charstream.open_stream file in
    let o = Filename.chop_extension file in
    Parse.parse s g;
    Charstream.close_stream s;
    Codegen.close_generator g;
    let _ = Sys.command ("nasm -f macho " ^ g.file) in
    let _ = Sys.command ("gcc -o " ^ o ^ " " ^ o ^ ".o") in
    ()
  with
    | Token.Syntax_error e ->
      Format.printf "syntax error: %s\n" e;
    | Sys_error _ ->
      print_string "no file found\n"

let help name = Format.printf "%s <file>\n" name

let () =
  if Array.length Sys.argv = 1
    then help (Array.get Sys.argv 0)
    else
      let file = Array.get Sys.argv 1 in
        Format.printf "compiling %s\n" file;
        compile file
