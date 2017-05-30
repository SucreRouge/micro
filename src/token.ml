type token = Begin
           | End
           | Identifier of string
           | Read
           | Write
           | Literal of int
           | Assign
           | LeftParen
           | RightParen
           | Add
           | Sub
           | Comma
           | Semicolon

type scanner = { mutable last_token: token option; stm: Charstream.stream }

exception Syntax_error of string

let syntax_error s msg =
  raise (Syntax_error (msg ^" on line " ^ (string_of_int s.stm.line_num)))

let rec skip_blank_chars stm =
  let c = Charstream.read_char stm in
    match c with
      | ' ' | '\t' | '\r' | '\n' -> skip_blank_chars stm
      | _ -> Charstream.unread_char stm c; ()

let scan s =
  let stm = s.stm in
  let c = Charstream.read_char stm in
  let rec scan_iden acc =
    let nc = Charstream.read_char stm in
      if Charstream.is_alpha nc || Charstream.is_digit nc || nc='_'
         then scan_iden (acc ^ (Char.escaped nc))
         else let _ = Charstream.unread_char stm nc in
              let lc = String.lowercase_ascii acc in
              if lc = "begin" then Begin
              else if lc = "end" then End
              else if lc = "read" then Read
              else if lc = "write" then Write
              else Identifier acc
   in
   let rec scan_lit acc =
    let nc = Charstream.read_char stm in
        if Charstream.is_digit nc
        then scan_lit (acc ^ (Char.escaped nc))
        else let _ = Charstream.unread_char stm nc in
             Literal (int_of_string acc)
   in
   if Charstream.is_alpha c then scan_iden (Char.escaped c)
   else if Charstream.is_digit c then scan_lit (Char.escaped c)
   else if c='+' then Add
   else if c='-' then Sub
   else if c=',' then Comma
   else if c=';' then Semicolon
   else if c='(' then LeftParen
   else if c=')' then RightParen
   else if c=':' && Charstream.read_char stm = '=' then Assign
   else syntax_error s "Could not identify token"


let new_scanner stm = { last_token=None; stm=stm }

let match_next s =
  match s.last_token with
    | None -> let _ = skip_blank_chars s.stm in scan s
    | Some tn -> s.last_token <- None; tn

let match_token s t = match_next s = t

let next_token s =
  match s.last_token with
    | None ->
      (skip_blank_chars s.stm;
       let t = scan s in
       s.last_token <- Some t; t)
    | Some t -> t
