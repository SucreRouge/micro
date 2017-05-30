type generator = { vars: (string, int) Hashtbl.t; file: string; chan: out_channel }

let new_generator file =
  let fs = (Filename.chop_extension file) ^ ".s" in
    { vars=Hashtbl.create 100; file=fs; chan=open_out fs }

let close_generator g = close_out g.chan

let gen g v = output_string g.chan v; output_string g.chan "\n"

let bottom_var _ g =
  Hashtbl.fold (fun _ v c -> if v >= c then (v+4) else c) g.vars 0

let empty_var s g i = (bottom_var s g) + 4 * (i - 1)

let var_addr s g v =
  if String.length v > 6 && String.sub v 0 6 = "__temp"
      then
        let i = String.sub v 6 ((String.length v) - 6) in "[esp+" ^ i ^ "]"
      else
        try "[esp+" ^ string_of_int (Hashtbl.find g.vars v) ^ "]"
        with Not_found -> syntax_error s ("identifier " ^ v ^ " not defined")

let var s g v = "dword " ^ (var_addr s g v)

let temp_var s g i =
  Token.Identifier ("__temp" ^ (string_of_int (empty_var s g i)))

let is_alloc_var _ g v = Hashtbl.mem g.vars v

let alloc_var s g v =
  if is_alloc_var s g v
    then var s g v
    else let _ = Hashtbl.replace g.vars v (empty_var s g 1) in var s g v

let token_var s g v =
  match v with
    | Token.Identifier i -> var s g i
    | _ -> syntax_error s "identifier expected"

let unop g opcode a = gen g ("    " ^ opcode ^ " " ^ a)

let binop g opcode a b = gen g ("    " ^ opcode ^ " " ^ a ^ ", " ^ b)

let push g a = op g "push" a


