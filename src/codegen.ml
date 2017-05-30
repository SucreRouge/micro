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
        with Not_found -> Token.syntax_error s ("identifier " ^ v ^ " not defined")

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
    | _ -> Token.syntax_error s "identifier expected"

let unop g opcode a = gen g ("    " ^ opcode ^ " " ^ a)

let binop g opcode a b = gen g ("    " ^ opcode ^ " " ^ a ^ ", " ^ b)

let push g a = unop g "push" a


let generate_begin _ g = gen g
"extern printf\n\
extern scanf\n\
\n\
section .data\n\
    inf: db '%d', 0\n\
    ouf: db '%d', 10, 0\n\
\n\
section .text\n\
    global main\n\
\n\
main:\n\
    sub   esp, 1000"

let generate_end _ g = gen g
"    add   esp, 1000\n\
exit:\n\
    mov  eax, 1 ; sys_exit\n\
    mov  ebx, 0\n\
    int  80h"

let generate_read s g id =
  match id with
    | Token.Identifier i ->
       binop g "lea" "eax" (var_addr s g i);
       push g "eax";
       push g "inf";
       unop g "call" "scanf";
       binop g "add " "esp" "8"
    | _ -> Token.syntax_error s "generate read called with invalid argument"

let generate_reads s g = List.iter (generate_read s g)

let generate_write s g id =
  match id with
    | Token.Identifier i ->
       push g (var s g i);
       push g "ouf";
       unop g "call" "printf";
       binop g "add " "esp" "8"
    | _ -> Token.syntax_error s "generate write called with invalid argument"

let generate_copy s g a b =
  match a with
    | Token.Identifier i ->
      (match b with
        | Token.Identifier i2 ->
            binop g "mov " "eax" (var s g i2);
            binop g "mov " (var s g i) "eax"
        | Token.Literal l -> binop g "mov " (var s g i) (string_of_int l)
        | _ -> Token.syntax_error s "generate copy called with invalid argument")
    | _ -> Token.syntax_error s "generate copy called with invalid argument"

let generate_assign s g a b =
  match a with
    | Token.Identifier i -> let _ = alloc_var s g i in generate_copy s g a b
    | _ -> Token.syntax_error s "generate assign called with invalid argument"

let generate_add s g d id1 id2 =
  match (id1, id2) with
   | (Token.Identifier _, Token.Identifier i2) ->
        let v = temp_var s g d in
        let vi = token_var s g v in
        let _ = generate_copy s g v id1 in
        let _ = binop g "add " vi (var s g i2) in v
   | (Token.Identifier _, Token.Literal l2) ->
        let v = temp_var s g d in
        let vi = token_var s g v in
        let _ = generate_copy s g v id1 in
        let _ = binop g "add " vi (string_of_int l2) in v
   | _ -> Token.syntax_error s "generate exp called with invalid argument"

let generate_sub s g d id1 id2 =
  match (id1, id2) with
   | (Token.Identifier _, Token.Identifier i2) ->
      let v = temp_var s g d in
      let vi = token_var s g v in
      let _ = generate_copy s g v id1 in
      let _ = binop g "sub " vi (var s g i2) in v
   | (Token.Identifier _, Token.Literal l2) ->
      let v = temp_var s g d in
      let vi = token_var s g v in
      let _ = generate_copy s g v id1 in
      let _ = binop g "sub " vi (string_of_int l2) in v
   | (Token.Literal _, Token.Identifier i2) ->
      let v = temp_var s g d in
      let vi = token_var s g v in
      let _ = generate_copy s g v id1 in
      let _ = binop g "sub " vi (var s g i2) in v
   | _ -> Token.syntax_error s "generate exp called with invalid argument"
