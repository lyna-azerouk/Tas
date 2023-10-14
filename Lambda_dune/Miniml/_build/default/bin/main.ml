(* ***EXEMPLES*** *)  
(**

let ex_nat2 : pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
let ex_omega : pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : pterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = inference ex_nat3
let ex_sous : pterm = Sou((N 3), (N 3))
let ex_sous_string : string =  inference ex_sous
let ex_listP1 : pterm = ListP (Cons(ex_id, Cons (ex_sous, Vide)))
let ex_listP2 : pterm = ListP (Cons( (N 3), Cons (ex_listP1, Vide)))
let ex_listP_string  : string = inference  ex_listP1
let ex_hd : pterm =  Hd(ex_listP1)
let ex_hd_string  : string = inference  ex_hd
let ex_Tail : pterm =  Tail ex_listP1
let ex_Tail_string  : string = inference  ex_Tail
let ex_izeo : pterm= Izte ((N 0), ex_listP1, ex_listP1)
let ex_zero_string : string = inference ex_izeo
let convertie : pterm = alpha_conv ex_nat1  "x" "y"
let convertie_string : string = inference convertie
*)
let ex_id : Typeur.pterm = Typeur.Abs ("x", Typeur.Var "x") 
let inf_ex_id : string = Typeur.inference ex_id 
let ex_k : Typeur.pterm = Typeur.Abs ("x", Typeur.Abs ("y", Typeur.Var "x")) 
let inf_ex_k : string = Typeur.inference ex_k
let ex_nat1 : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.Add(Typeur.Var "x", Typeur.N 1)),Typeur.N 3)
let inf_ex_nat1 : string = Typeur.inference ex_nat1
let ex_s : Typeur.pterm = Typeur.Abs ("x", Typeur.Abs ("y", Typeur.Abs ("z", Typeur.App (Typeur.App (Typeur.Var "x", Typeur.Var "z"), Typeur.App (Typeur.Var "y", Typeur.Var "z")))))
let chat_exemple : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.App (Typeur.Var "x", Typeur.Var "x")), Typeur.N 42)
let inf_ex_s : string = Typeur.inference ex_s 
let exemple_let: Typeur.pterm = Typeur.Let("x", (Typeur.Var "y" ), (Typeur.Var "x"))
let convertie_bis = Typeur.alpha_conv_bis exemple_let  []
let exemple = Typeur.alpha_conv_bis ex_s []
let exemeple_reduction : Typeur.pterm = Typeur.reduction chat_exemple

let main () =
  print_endline "======================";
  print_endline inf_ex_id;
  print_endline "======================";
  print_endline inf_ex_k;
  print_endline "======================";
  print_endline inf_ex_s;
  print_endline "======================";
  (**print_endline inf_ex_omega;
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
  print_endline "======================";*)
  print_endline (Typeur.print_term exemple_let );
  print_endline (Typeur.print_term convertie_bis);
  print_endline "======================";
  print_endline (Typeur.print_term chat_exemple);
  print_endline (Typeur.print_term exemeple_reduction )


  
 
 let _ = main ()

 

(* si reduce alors si non app alors on ne fait rient pas de reduction sinon (si App) on fait substitution on ne renvoie que la partie droite du App donc le body du app *)