open Miniml;;

(* ***EXEMPLES*** *)  
let ex_id : pterm = Abs ("x", Var "x") 
let inf_ex_id : string = inference ex_id 
let ex_k : pterm = Abs ("x", Abs ("y", Var "x")) 
let inf_ex_k : string = inference ex_k
let ex_s : pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = inference ex_s 
let ex_nat1 : pterm = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = inference ex_nat1
let ex_nat2 : pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
let ex_omega : pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let chat_exemple : pterm = App (Abs ("x", App (Var "x", Var "y")), N 42)
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : pterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = inference ex_nat3
let ex_sous : pterm = Sou((N 3), (N 3))
let ex_sous_string : string =  inference ex_sous
let ex_listP1 : pterm = ListP (Cons(ex_id, Cons (ex_sous, Vide)))
let ex_listP2 : pterm = ListP (Cons( (N 3), Cons (ex_listP1, Vide)))
let ex_listP_string  : string = inference  ex_listP1
let ex_hd : pterm =  Hd(ex_listP1)
let ab_exemple : pterm =  App (Abs ("x", App (Var "x", Var "x")),  (Abs ("x", App (Var "x", Var "x"))))
let ex_hd_string  : string = inference  ex_hd
let ex_Tail : pterm =  Tail ex_listP1
let ex_Tail_string  : string = inference  ex_Tail
let ex_izeo : pterm= Izte ((N 0), ex_listP1, ex_listP1)
let ex_zero_string : string = inference ex_izeo
let convertie : pterm = alpha_conv ex_nat1  "x" "y"
let convertie_string : string = inference convertie
let convertie_bis : pterm = alpha_conv_bis ex_listP1  []
let convertie_string : string = inference convertie_bis 
let exemple = alpha_conv_bis ex_s []
let exemple_string : string = print_term exemple
let exemeple_reduction : pterm = reduction ex_s

let main () =
  print_endline "======================";
  print_endline inf_ex_id;
  print_endline "======================";
  print_endline inf_ex_k;
  print_endline "======================";
  print_endline inf_ex_s;
  print_endline "======================";
  print_endline inf_ex_omega;
  print_endline "======================";
  print_endline inf_ex_nat1;
  print_endline "======================";
  print_endline inf_ex_nat2;
  print_endline "======================";
  print_endline inf_ex_nat3;
  print_endline "======================";
  print_endline ex_sous_string;
  print_endline "======================";
  print_endline ex_listP_string;
  print_endline "======================";
  print_endline ex_Tail_string;
  print_endline "======================";
  print_endline ex_hd_string;
  print_endline "======================";
  print_endline ex_zero_string;
  print_endline "======================";
  print_endline (print_term ex_omega);
  print_endline "======================";
  print_endline (print_term ex_listP1);
  print_endline (print_term convertie_bis);
  print_endline "======================";
  print_endline (print_term ex_s);
  print_endline (print_term exemeple_reduction )


  
 
 let _ = main ()

 

(* si reduce alors si non app alors on ne fait rient pas de reduction sinon (si App) on fait substitution on ne renvoie que la partie droite du App donc le body du app *)