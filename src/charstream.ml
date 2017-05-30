type stream = { mutable chr: char option; mutable line_num: int; chan: in_channel }

let open_stream file = { chr=None; line_num=1; chan=open_in file }

let close_stream stm = close_in stm.chan

let read_char stm =
  match stm.chr with 
    | None ->
      let c = input_char stm.chan in
        if c = '\n'
          then let _ = stm.line_num <- stm.line_num + 1 in c
          else c
    | Some c -> stm.chr <- None; c

let unread_char stm c = stm.chr <- Some c

let is_digit c =
  let code = Char.code c in code >= Char.code('0') && code <= Char.code('9')

let is_alpha c =
  let code = Char.code c in
    (code >= Char.code('A') && code <= Char.code('Z')) ||
    (code >= Char.code('a') && code <= Char.code('z'))
