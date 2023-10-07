# 3 "alex.mll"
 
     open Util;;
(**) open Asyn;;


(* The table of keywords *)

let keyword_table = (Hashtbl.create 100 : (string, token) Hashtbl.t)
;;

do_list (fun (str,tok) -> Hashtbl.add keyword_table str tok) [
  "else", ELSE;
  "function", FUNCTION;
  "if", IF;
  "in", IN;
  "let", LET;
  "rec", REC;
  "ref", REF;
  "then", THEN
];;

let add_infix s =
  Hashtbl.add keyword_table s (INFIX s)
;;

let remove_infix s =
  Hashtbl.remove keyword_table s
;;


(* To buffer string literals *)

let initial_string_buffer = create_string 256;;
let string_buff = ref initial_string_buffer;;
let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()
;;

let store_string_char c =
  if !string_index >= string_length (!string_buff) then begin
    let new_buff = create_string (string_length (!string_buff) * 2) in
      blit_string (!string_buff) 0 new_buff 0 (string_length (!string_buff));
      string_buff := new_buff
  end;
  set_nth_char (!string_buff) (!string_index) c;
  incr string_index
;;

let get_stored_string () =
  let s = sub_string (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s
;;

(* To translate escape sequences *)

let char_for_backslash = function

    'n' -> '\010'
  | 'r' -> '\013'

  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c
;;

let char_for_decimal_code lexbuf i =
  let c = 
    100 * (int_of_char(get_lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(get_lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(get_lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)
;;


# 82 "alex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\233\255\234\255\092\000\127\000\164\000\199\000\234\000\
    \240\255\241\255\013\001\033\000\075\000\048\001\248\255\249\255\
    \250\255\251\255\067\001\097\001\170\001\007\000\245\001\069\002\
    \092\000\047\000\127\000\079\002\102\002\077\001\083\000\134\002\
    \157\002\182\002\245\255\246\255\243\255\217\002\158\000\250\255\
    \251\255\238\002\255\255\182\002\253\255\014\000\162\000\248\002\
    \252\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\020\000\020\000\018\000\017\000\016\000\
    \255\255\255\255\013\000\011\000\022\000\018\000\255\255\255\255\
    \255\255\255\255\002\000\002\000\001\000\000\000\255\255\255\255\
    \003\000\255\255\255\255\255\255\002\000\002\000\002\000\003\000\
    \255\255\008\000\255\255\255\255\255\255\019\000\255\255\255\255\
    \255\255\005\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\000\000\255\255\039\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\255\255\255\255\255\255\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\021\000\021\000\000\000\021\000\021\000\000\000\000\000\
    \021\000\021\000\000\000\021\000\021\000\000\000\000\000\046\000\
    \046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \021\000\007\000\017\000\000\000\006\000\003\000\006\000\021\000\
    \016\000\015\000\004\000\005\000\014\000\013\000\046\000\003\000\
    \019\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\012\000\011\000\006\000\010\000\006\000\007\000\
    \006\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\009\000\036\000\008\000\006\000\030\000\
    \030\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\000\000\006\000\003\000\006\000\000\000\
    \003\000\003\000\003\000\030\000\030\000\035\000\003\000\003\000\
    \034\000\003\000\003\000\003\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \003\000\023\000\000\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\037\000\003\000\046\000\003\000\003\000\003\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \042\000\023\000\046\000\000\000\000\000\005\000\000\000\000\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\005\000\005\000\
    \000\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\000\000\000\000\003\000\005\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\006\000\006\000\006\000\000\000\000\000\
    \000\000\006\000\006\000\000\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\041\000\003\000\000\000\003\000\000\000\000\000\
    \002\000\006\000\005\000\006\000\006\000\006\000\006\000\006\000\
    \000\000\000\000\000\000\007\000\000\000\000\000\007\000\007\000\
    \007\000\000\000\000\000\000\000\007\000\007\000\000\000\007\000\
    \007\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\000\000\005\000\000\000\007\000\006\000\007\000\007\000\
    \007\000\007\000\007\000\000\000\000\000\000\000\006\000\000\000\
    \000\000\006\000\006\000\006\000\000\000\000\000\000\000\006\000\
    \006\000\000\000\006\000\006\000\006\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\006\000\000\000\006\000\
    \007\000\006\000\006\000\006\000\006\000\006\000\000\000\000\000\
    \000\000\005\000\000\000\000\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\005\000\005\000\000\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
    \007\000\000\000\005\000\006\000\005\000\005\000\033\000\005\000\
    \005\000\024\000\000\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\000\000\000\000\000\000\
    \023\000\006\000\000\000\006\000\000\000\000\000\005\000\024\000\
    \000\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\000\000\000\000\000\000\040\000\000\000\
    \000\000\000\000\000\000\025\000\000\000\000\000\023\000\000\000\
    \023\000\000\000\000\000\000\000\005\000\000\000\005\000\000\000\
    \026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\025\000\000\000\000\000\023\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\000\000\000\000\000\000\
    \000\000\022\000\000\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \032\000\000\000\032\000\000\000\000\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\000\000\000\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\005\000\
    \000\000\000\000\005\000\005\000\005\000\000\000\000\000\000\000\
    \005\000\005\000\000\000\005\000\005\000\005\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \005\000\000\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \046\000\000\000\037\000\045\000\000\000\037\000\037\000\037\000\
    \000\000\000\000\000\000\037\000\037\000\000\000\037\000\037\000\
    \037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \044\000\000\000\000\000\037\000\005\000\037\000\037\000\037\000\
    \037\000\037\000\000\000\000\000\000\000\000\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\005\000\000\000\005\000\000\000\000\000\037\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
    \044\000\000\000\000\000\000\000\000\000\037\000\000\000\037\000\
    \000\000\000\000\000\000\000\000\044\000\000\000\000\000\000\000\
    \044\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\255\255\255\255\
    \021\000\021\000\255\255\021\000\021\000\255\255\255\255\045\000\
    \045\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\021\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\045\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\011\000\000\000\000\000\025\000\
    \025\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\003\000\000\000\255\255\
    \003\000\003\000\003\000\030\000\030\000\012\000\003\000\003\000\
    \012\000\003\000\003\000\003\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\003\000\255\255\
    \003\000\003\000\003\000\003\000\003\000\255\255\255\255\255\255\
    \004\000\024\000\255\255\004\000\004\000\004\000\255\255\255\255\
    \255\255\004\000\004\000\046\000\004\000\004\000\004\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\255\255\
    \255\255\004\000\003\000\004\000\004\000\004\000\004\000\004\000\
    \038\000\024\000\046\000\255\255\255\255\005\000\255\255\255\255\
    \005\000\005\000\005\000\255\255\255\255\255\255\005\000\005\000\
    \255\255\005\000\005\000\005\000\255\255\255\255\255\255\255\255\
    \003\000\255\255\003\000\255\255\255\255\004\000\005\000\255\255\
    \005\000\005\000\005\000\005\000\005\000\255\255\255\255\255\255\
    \006\000\255\255\255\255\006\000\006\000\006\000\255\255\255\255\
    \255\255\006\000\006\000\255\255\006\000\006\000\006\000\255\255\
    \255\255\255\255\038\000\004\000\255\255\004\000\255\255\255\255\
    \000\000\006\000\005\000\006\000\006\000\006\000\006\000\006\000\
    \255\255\255\255\255\255\007\000\255\255\255\255\007\000\007\000\
    \007\000\255\255\255\255\255\255\007\000\007\000\255\255\007\000\
    \007\000\007\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \005\000\255\255\005\000\255\255\007\000\006\000\007\000\007\000\
    \007\000\007\000\007\000\255\255\255\255\255\255\010\000\255\255\
    \255\255\010\000\010\000\010\000\255\255\255\255\255\255\010\000\
    \010\000\255\255\010\000\010\000\010\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\006\000\255\255\006\000\255\255\010\000\
    \007\000\010\000\010\000\010\000\010\000\010\000\255\255\255\255\
    \255\255\013\000\255\255\255\255\013\000\013\000\013\000\255\255\
    \255\255\255\255\013\000\013\000\255\255\013\000\013\000\013\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\007\000\255\255\
    \007\000\255\255\013\000\010\000\013\000\013\000\013\000\013\000\
    \013\000\018\000\255\255\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\255\255\255\255\255\255\
    \018\000\010\000\255\255\010\000\255\255\255\255\013\000\019\000\
    \255\255\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\038\000\255\255\
    \255\255\255\255\255\255\019\000\255\255\255\255\019\000\255\255\
    \018\000\255\255\255\255\255\255\013\000\255\255\013\000\255\255\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\019\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\019\000\255\255\255\255\019\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\019\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\255\255\255\255\255\255\
    \255\255\020\000\255\255\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \023\000\255\255\023\000\255\255\255\255\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \027\000\027\000\027\000\027\000\027\000\027\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\028\000\
    \028\000\028\000\028\000\028\000\028\000\255\255\255\255\255\255\
    \027\000\027\000\027\000\027\000\027\000\027\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\028\000\
    \028\000\028\000\028\000\028\000\028\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\033\000\
    \255\255\255\255\033\000\033\000\033\000\255\255\255\255\255\255\
    \033\000\033\000\255\255\033\000\033\000\033\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \033\000\255\255\033\000\033\000\033\000\033\000\033\000\255\255\
    \041\000\255\255\037\000\041\000\255\255\037\000\037\000\037\000\
    \255\255\255\255\255\255\037\000\037\000\255\255\037\000\037\000\
    \037\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \041\000\255\255\255\255\037\000\033\000\037\000\037\000\037\000\
    \037\000\037\000\255\255\255\255\255\255\255\255\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\033\000\255\255\033\000\255\255\255\255\037\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\041\000\255\255\255\255\255\255\255\255\255\255\
    \041\000\255\255\255\255\255\255\255\255\037\000\255\255\037\000\
    \255\255\255\255\255\255\255\255\041\000\255\255\255\255\255\255\
    \041\000\255\255\041\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec main lexbuf =
   __ocaml_lex_main_rec lexbuf 0
and __ocaml_lex_main_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 90 "alex.mll"
      ( main lexbuf )
# 387 "alex.ml"

  | 1 ->
# 92 "alex.mll"
      ( let s = get_lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
             IDENT s )
# 396 "alex.ml"

  | 2 ->
# 101 "alex.mll"
      ( INT (int_of_string(get_lexeme lexbuf)) )
# 401 "alex.ml"

  | 3 ->
# 103 "alex.mll"
      ( FLOAT (float_of_string(get_lexeme lexbuf)) )
# 406 "alex.ml"

  | 4 ->
# 105 "alex.mll"
      ( reset_string_buffer();
        let string_start = lexbuf.Lexing.lex_start_pos + lexbuf.Lexing.lex_abs_pos in
        begin try
          string lexbuf
        with Error(Unterminated_string, _, string_end) ->
          raise(Error(Unterminated_string, string_start, string_end))
        end;
        lexbuf.Lexing.lex_start_pos <- string_start - lexbuf.Lexing.lex_abs_pos;
        STRING (Bytes.to_string (get_stored_string())) )
# 419 "alex.ml"

  | 5 ->
# 116 "alex.mll"
        ( LPAREN )
# 424 "alex.ml"

  | 6 ->
# 117 "alex.mll"
        ( RPAREN )
# 429 "alex.ml"

  | 7 ->
# 118 "alex.mll"
        ( COMMA )
# 434 "alex.ml"

  | 8 ->
# 119 "alex.mll"
         ( MINUSGREATER )
# 439 "alex.ml"

  | 9 ->
# 120 "alex.mll"
         ( COLONCOLON )
# 444 "alex.ml"

  | 10 ->
# 121 "alex.mll"
         ( COLONEQUAL )
# 449 "alex.ml"

  | 11 ->
# 122 "alex.mll"
        ( SEMI )
# 454 "alex.ml"

  | 12 ->
# 123 "alex.mll"
         ( SEMISEMI )
# 459 "alex.ml"

  | 13 ->
# 124 "alex.mll"
        ( EQUAL )
# 464 "alex.ml"

  | 14 ->
# 125 "alex.mll"
        ( LBRACKET )
# 469 "alex.ml"

  | 15 ->
# 126 "alex.mll"
        ( RBRACKET )
# 474 "alex.ml"

  | 16 ->
# 130 "alex.mll"
            ( PREFIX(get_lexeme lexbuf) )
# 479 "alex.ml"

  | 17 ->
# 133 "alex.mll"
            ( INFIX(get_lexeme lexbuf) )
# 484 "alex.ml"

  | 18 ->
# 135 "alex.mll"
            ( INFIX(get_lexeme lexbuf) )
# 489 "alex.ml"

  | 19 ->
# 137 "alex.mll"
            ( INFIX(get_lexeme lexbuf) )
# 494 "alex.ml"

  | 20 ->
# 139 "alex.mll"
            ( INFIX(get_lexeme lexbuf) )
# 499 "alex.ml"

  | 21 ->
# 140 "alex.mll"
         ( EOF )
# 504 "alex.ml"

  | 22 ->
# 142 "alex.mll"
      ( raise (Error(Illegal_character,
                            Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) )
# 510 "alex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_main_rec lexbuf __ocaml_lex_state

and string lexbuf =
   __ocaml_lex_string_rec lexbuf 38
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 148 "alex.mll"
      ( () )
# 522 "alex.ml"

  | 1 ->
# 150 "alex.mll"
      ( string lexbuf )
# 527 "alex.ml"

  | 2 ->
# 152 "alex.mll"
      ( store_string_char(char_for_backslash(get_lexeme_char lexbuf 1));
        string lexbuf )
# 533 "alex.ml"

  | 3 ->
# 155 "alex.mll"
      ( store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf )
# 539 "alex.ml"

  | 4 ->
# 158 "alex.mll"
      ( raise (Error
                (Unterminated_string, 0, Lexing.lexeme_start lexbuf)) )
# 545 "alex.ml"

  | 5 ->
# 161 "alex.mll"
      ( store_string_char(get_lexeme_char lexbuf 0);
        string lexbuf )
# 551 "alex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec lexbuf __ocaml_lex_state

;;

