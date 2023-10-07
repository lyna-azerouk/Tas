(* version 2022/2023 *)
1 ;;
let un = 1 ;;
let moinsun = 0 - un ;;
let pi = 3.14 ;;

let l = 1.2 :: 3.4 :: 2.6 :: [] ;;
hd l ;;

let succ = function x -> x + 1 ;;
succ 1 ;;
succ pi ;;

let estneg = function x -> if x < 0  then true else false ;;
estneg un ;;
estneg moinsun ;;

let rec fact = function x -> if x = 0 then 1 else x * (fact (x-1)) ;; 
fact un ;;

let rec long = function l -> if l = [] then 0 else 1 + (long(tl l)) ;;
long l ;;

let compose = function f -> function g -> function x -> f ( g x ) ;;
let myfun = compose estneg succ ;;
myfun 1;;
let myfun2 = compose succ succ ;;
myfun2 1 ;;


ref 3 ;;

let x = ref 4 ;;
3
let incr = function x -> x := !x + 1;;

incr x ;;

let r = let f = ref (function x -> x) in let a = f:= function x -> x + 1 in (!f) true;;

(**

./intertypeur
Master STL - M2 - Module TAS - 2022/2023 

synthetiseur de types a` la ML

$ let c = function x -> ( !x, !x ) ;;
Erreur No unification for ref type
$ $ ^C
[micmacair:tp/v2/dist] emmanuel% ./intertypeur
Master STL - M2 - Module TAS - 2022/2023 

synthetiseur de types a` la ML

$ 1 ;;
- : int
$ let un = 1 ;;
un : int
$ let moinsun = 0 - un ;;
moinsun : int
$ let pi = 3.14 ;;
pi : float
$ let l = 1.2 :: 3.4 :: 2.6 :: [] ;;
l : ((float) list)
$ hd l ;;
- : float
$ let succ = function x -> x + 1 ;;
succ : (int -> int)
$ succ 1 ;;
- : int
$ succ pi ;;
Type clash between int and float
Erreur de typage
$ let estneg = function x -> if x < 0  then true else false ;;
estneg : (int -> bool)
$ estneg un ;;
- : bool
$ estneg moinsun ;;
- : bool
$ let rec fact = function x -> if x = 0 then 1 else x * (fact (x-1)) ;; 
fact : (int -> int)
$ fact un ;;
- : int
$ let rec long = function l -> if l = [] then 0 else 1 + (long(tl l)) ;;
long : ((('a) list) -> int)
$ long l ;;
- : int
$ let compose = function f -> function g -> function x -> f ( g x ) ;;
compose : (('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))
$ let myfun = compose estneg succ ;;
myfun : (int -> bool)
$ myfun 1;;
- : bool
$ let myfun2 = compose succ succ ;;
myfun2 : (int -> int)
$ myfun2 1 ;;
- : int
$ ref 3 ;;
- : ((int) ref)
$ let x = ref 4 ;;
x : ((int) ref)
$ let incr = function x -> x := !x + 1;;
incr : (((int) ref) -> unit)
$ incr x;;
- : unit
$ let r = let f = ref (function x -> x) in let a = f:= function x -> x + 1 in (!f) true;;
Type clash between int and bool
Erreur de typage
*)
