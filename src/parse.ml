let add s g d l r =
  match (l, r) with
    | (Token.Literal l1,Token.Literal l2) ->Token.Literal (l1+l2)
    | (Token.Identifier _, Token.Literal _) -> Codegen.generate_add s g d l r
    | (Token.Literal _, Token.Identifier _) -> Codegen.generate_add s g d r l
    | _ -> Token.syntax_error s "expected literal or identifier for add operation"

let sub s g d l r =
  match (l, r) with
    | (Token.Literal l1,Token.Literal l2) ->Token.Literal (l1-l2)
    | (Token.Identifier _, Token.Literal _) -> Codegen.generate_sub s g d l r
    | (Token.Literal _, Token.Identifier _) -> Codegen.generate_sub s g d l r
    | _ -> Token.syntax_error s "expected literal or identifier for sub operation"

let mul s g d l r =
  match (l, r) with
    | (Token.Literal l1,Token.Literal l2) ->Token.Literal (l1*l2)
    | (Token.Identifier _, Token.Literal _) -> Codegen.generate_mul s g d l r
    | (Token.Literal _, Token.Identifier _) -> Codegen.generate_mul s g d r l
    | _ -> Token.syntax_error s "expected literal or identifier for mul operation"

let div s g d l r =
  match (l, r) with
    | (Token.Literal l1,Token.Literal l2) ->Token.Literal (l1/l2)
    | (Token.Identifier _, Token.Literal _) -> Codegen.generate_div s g d l r
    | (Token.Literal _, Token.Identifier _) -> Codegen.generate_div s g d l r
    | _ -> Token.syntax_error s "expected literal or identifier for div operation"

let rec expression s g d =
  let primary s =
    match Token.next_token s with
      | Token.LeftParen ->
        let _ = Token.match_token s Token.LeftParen in
        let e = expression s g (d+1) in
        if Token.match_token s Token.RightParen
          then Some e
          else Token.syntax_error s "right paren expected in expression"
      | Token.Identifier i ->
        let _ = Token.match_token s (Token.Identifier i) in
        Some (Token.Identifier i)
      | Token.Literal l ->
        let _ = Token.match_token s (Token.Literal l) in
        Some (Token.Literal l)
      | _ -> None
  in
  let lp = primary s in
  match lp with
    | Some l ->
      (match Token.next_token s with
         | Token.Add ->
            let _ = Token.match_token s Add in
            add s g d l (expression s g (d+1))
         | Token.Sub ->
            let _ = Token.match_token s Sub in
            sub s g d l (expression s g (d+1))
         | Token.Mul ->
            let _ = Token.match_token s Mul in
            mul s g d l (expression s g (d+1))
         | Token.Div ->
            let _ = Token.match_token s Div in
            div s g d l (expression s g (d+1))
         | _ -> l)
    | None -> Token.syntax_error s "literal or identifier expected"

let write s g =
  let rec expressions c =
    let e = expression s g 1 in
    if match e with
      | Token.Identifier _ -> let _ = Codegen.generate_write s g e in true
      | Token.Literal _ -> let _ = Codegen.generate_write s g e in true
      | _ -> false
    then if (Token.next_token s) = Token.Comma
      then let _ = Token.match_token s Token.Comma in expressions (c+1)
      else (c+1)
    else c
  in
  if Token.match_token s Token.Write then
      if Token.match_token s Token.LeftParen then
          if expressions 0 > 0 then
              if Token.match_token s Token.RightParen then true
              else Token.syntax_error s "right paren expected in write statement"
          else Token.syntax_error s "write statement expects at least one expression"
      else Token.syntax_error s "left paren expected in write statement"
  else Token.syntax_error s "write statement expected"

let identifiers s =
  let rec idens ids =
      match Token.next_token s with
        | Token.Identifier i ->
          let _ = Token.match_next s in
          let n = Token.next_token s in
          if n = Token.Comma
            then let _ = Token.match_token s Token.Comma in idens (Token.Identifier i :: ids)
            else idens (Token.Identifier i :: ids)
        | _ -> ids
  in idens []

let read s g =
  if Token.match_token s Token.Read then
      if Token.match_token s Token.LeftParen then
          let ids = identifiers s in
          if ids = []
            then Token.syntax_error s "read statement expects comma seperated identifier(s)"
          else if Token.match_token s Token.RightParen
            then let _ = Codegen.generate_reads s g (List.rev ids) in true
          else Token.syntax_error s "right paren expected in read statement"
      else Token.syntax_error s "left paren expected in read statement"
   else Token.syntax_error s "read statement expected"

let assignment s g =
  let id = Token.match_next s in
     match id with
        Token.Identifier i -> 
          if Token.match_token s Token.Assign
            then
               let new_var = if Codegen.is_alloc_var s g i then 0 else 1 in
               let id2 = expression s g (1+new_var) in
               match id2 with
                 | Token.Literal _ ->
                    let _ = Codegen.generate_assign s g id id2 in true
                 | Token.Identifier _ ->
                    let _ = Codegen.generate_assign s g id id2 in true
                 | _ -> Token.syntax_error s "literal or identifier expected"
           else Token.syntax_error s "assign symbol expected"
      | _ -> Token.syntax_error s "identifier expected"

let statement s g =
  let t = Token.next_token s in
    if match t with
      | Token.Read -> read s g
      | Token.Write -> write s g
      | Token.Identifier _ -> assignment s g
      | _ -> false
    then
        if Token.match_token s Token.Semicolon then true
        else Token.syntax_error s "statement must end with semicolon"
    else false

let rec statements s g = if statement s g then statements s g else ()

let program s g =
  if Token.match_token s Token.Begin then
      let _ = Codegen.generate_begin s g in
      let _ = statements s g in
      if Token.match_token s Token.End then
      let _ = Codegen.generate_end s g in ()
      else Token.syntax_error s "program should end with end keyword"
  else Token.syntax_error s "program should start with begin keyword"

let parse stm g =
  let s = (Token.new_scanner stm) in
      try program s g
      with End_of_file ->
        Token.syntax_error s "program reached end of file before end keyword"
