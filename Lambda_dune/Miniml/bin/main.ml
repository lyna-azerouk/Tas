open Typeur;;

(* ***EXEMPLES*** *)  
(**
let ex_nat2 : pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
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
let ex_k : Typeur.pterm = Typeur.Abs ("y", Typeur.Abs ("y", Typeur.Var "x")) 
let ex_nat1 : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.Add(Typeur.Var "x", Typeur.N 1)),Typeur.N 3)
let inf_ex_nat1 : string = Typeur.inference ex_nat1
let ex_s : Typeur.pterm = Typeur.Abs ("x", Typeur.Abs ("y", Typeur.Abs ("z", Typeur.App (Typeur.App (Typeur.Var "x", Typeur.Var "z"), Typeur.App (Typeur.Var "y", Typeur.Var "z")))))
let ex_omega : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.App (Typeur.Var "x", Typeur.Var "x")), Typeur.Abs ("y", Typeur.App (Typeur.Var "y",Typeur. Var "y")))
let chat_exemple : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.App (Typeur.Var "x", Typeur.Var "x")), Typeur.N 42)
let inf_ex_s : string = Typeur.inference ex_s 
let exemple_let: Typeur.pterm = Typeur.Let("x", (Typeur.Var "x" ), (Typeur.Var "x"))
let convertie_bis = Typeur.alpha_conv_bis ex_id  []
let exemple = Typeur.alpha_conv_bis ex_s []
let exemeple_reduction : Typeur.pterm = Typeur.reduction chat_exemple
let ex_sous : Typeur.pterm = Typeur.Sou((Typeur.N 3), (Typeur.N 3))
let ex_listP1 : Typeur.pterm = Typeur.ListP (Typeur.Cons(ex_id, Typeur.Cons ((N 0), Typeur.Vide)))
let ex_hd: Typeur.pterm = Typeur.Hd(ex_listP1)
let ex_hd_reduced : Typeur.pterm =Typeur.reduction ex_hd
let ex_tail: Typeur.pterm =Typeur.Tail(ex_listP1)
let ex_tail_reduce: Typeur.pterm = Typeur.reduction ex_tail
let ex_ifz : Typeur.pterm = Typeur.Izte((N 1), ex_hd, ex_sous)
let ex_ifz_reduce : Typeur.pterm = Typeur.reduction ex_ifz
let ex_ife :Typeur.pterm = Typeur.Iete(ex_listP1, ex_id, ex_sous)
let ex_ife_reduce : Typeur.pterm = Typeur.reduction ex_ife
let ex_let :Typeur.pterm =Typeur.Let("x", ex_id, ex_k)
let ex_let_reduce : Typeur.pterm = Typeur.reduction ex_let

let main () =
  (**print_endline inf_ex_omega;
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
  print_endline ex_zero_string;*)
  print_endline "======================";
  print_endline (Typeur.print_term ex_omega);
  print_endline "======================";
  print_endline (Typeur.print_term ex_id );
  print_endline (Typeur.print_term convertie_bis);
  print_endline "======================";
  print_endline (Typeur.print_term chat_exemple);
  print_endline (Typeur.print_term exemeple_reduction );
  print_endline "======================";
  print_endline (Typeur.print_term ex_hd);
  prerr_endline (Typeur.print_term ex_hd_reduced);
  print_endline "======================";
  print_endline (Typeur.print_term ex_tail);
  prerr_endline (Typeur.print_term ex_tail_reduce);
  print_endline "======================";
  print_endline (Typeur.print_term ex_ifz); 
  print_endline (Typeur.print_term ex_ifz_reduce);
  print_endline "======================";
  print_endline (Typeur.print_term ex_ife); 
  print_endline (Typeur.print_term ex_ife_reduce);
  print_endline "======================";
  print_endline (Typeur.print_term ex_let); 
  print_endline (Typeur.print_term ex_let_reduce);
  print_endline "==========TYPEUR============";
  print_endline (Typeur.print_term ex_id);
  print_endline inf_ex_id;
  print_endline "======================";
  print_endline (Typeur.print_term ex_s);
  print_endline inf_ex_s;
  print_endline "======================";
  print_endline (Typeur.print_term ex_nat1);
  print_endline inf_ex_nat1;
  print_endline "======================";
  print_endline (Typeur.print_term ex_listP1);
  print_endline (Typeur.inference ex_listP1)


 (** assert_equal  ~msg:"" ~printer:(Typeur.print_term ex_id (alpha_conv_bis ex_id [])) *)  
 
 let _ = main ()

 

(* si reduce alors si non app alors on ne fait rient pas de reduction sinon (si App) on fait substitution on ne renvoie que la partie droite du App donc le body du app *)