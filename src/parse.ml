let parse stm g =
  let s = (Token.new_scanner stm) in
      try
          Token.program s g
      with End_of_file ->
        Token.syntax_error s "program reached end of file before end keyword"

let program s g =
  if Token.match_token s Token.Begin then
      let _ = generate_begin s g in
      let _ = statements s g in
      if Token.match_token s Token.End then
      let _ = generate_end s g in ()
      else Token.syntax_error s "program should end with end keyword"
  else Token.syntax_error s "program should start with begin keyword"

let rec statements s g = if statement s g then statements s g else ()

let statement s g =
  let t = next_token s in
    if match t with
      | Token.Read -> read s g
      | Token.Write -> write s g
      | Token.Identifier i -> assignment s g
      | _ -> false
    then
        if Token.match_token s Token.Semicolon then true
        else Token.syntax_error s "statement must end with semicolon"
    else false
