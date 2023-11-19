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
let ex_k : Typeur.pterm = Typeur.Abs ("y", Typeur.Abs ("y", Typeur.Var "x")) 
let ex_nat1 : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.Add(Typeur.Var "x", Typeur.N 1)),Typeur.N 3)
let inf_ex_nat1 : string = Typeur.inference ex_nat1
let ex_s : Typeur.pterm = Typeur.Abs ("x", Typeur.Abs ("y", Typeur.Abs ("z", Typeur.App (Typeur.App (Typeur.Var "x", Typeur.Var "z"), Typeur.App (Typeur.Var "y", Typeur.Var "z")))))
let ex_omega : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.App (Typeur.Var "x", Typeur.Var "x")), Typeur.Abs ("x", Typeur.App (Typeur.Var "x",Typeur. Var "x")))
let chat_exemple : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.App (Typeur.Var "x", Typeur.Var "x")), Typeur.N 42)
let inf_ex_s : string = Typeur.inference ex_s 
let exemple_let: Typeur.pterm = Typeur.Let("x", (Typeur.Var "x" ), (Typeur.Var "x"))
let convertie_bis = Typeur.alpha_conv_bis ex_id  []
let exemple = Typeur.alpha_conv_bis ex_s []
let exemeple_reduction : Typeur.pterm = Typeur.reduction chat_exemple [] []
let ex_sous : Typeur.pterm = Typeur.Sou((Typeur.N 3), (Typeur.N 3))
let ex_listP1 : Typeur.pterm = Typeur.ListP (Typeur.Cons(N 3, Typeur.Cons ((N 0), Typeur.Vide)))
let ex_listP2 : Typeur.pterm = Typeur.ListP (Typeur.Cons((N 0), Typeur.Cons ((N 0), Typeur.Vide)))
let ex_listP3 : Typeur.pterm = Typeur.ListP (Typeur.Cons(ex_id, Typeur.Cons (ex_id, Typeur.Vide)))
let ex_listP4 : Typeur.pterm = Typeur.ListP (Typeur.Cons(ex_sous, Typeur.Cons (ex_sous, Typeur.Vide)))
let ex_listP5 : Typeur.pterm = Typeur.ListP (Typeur.Cons((N 0), Typeur.Cons ((N 0), Typeur.Vide)))
let ex_listP6: Typeur.pterm = Typeur.ListP (Typeur.Cons(ex_id, Typeur.Cons ((N 0), Typeur.Vide)))

let ex_hd: Typeur.pterm = Typeur.Hd(ex_listP1)
let ex_hd_reduced : Typeur.pterm =Typeur.reduction ex_hd [][]
let ex_tail: Typeur.pterm =Typeur.Tail(ex_listP1)
let ex_tail_reduce: Typeur.pterm = Typeur.reduction ex_tail [] []
let ex_ifz1 : Typeur.pterm = Typeur.Izte((N 1), ex_listP2, ex_listP4)
let ex_ifz2 : Typeur.pterm = Typeur.Izte((N 0), ex_sous, ex_sous)
let ex_ifz3 : Typeur.pterm = Typeur.Izte((N 0), ex_listP1, ex_sous)

let ex_ifz_reduce : Typeur.pterm = Typeur.reduction ex_ifz1 [] []
let ex_ife :Typeur.pterm = Typeur.Iete(ex_listP1, ex_id, ex_sous)
let ex_ife_reduce : Typeur.pterm = Typeur.reduction ex_ife [] []
let ex_let :Typeur.pterm =Typeur.Let("x", ex_id, ex_k)
let ex_let_ab : Typeur.pterm = Typeur.Let ("x", Typeur.N 2, Typeur.App (Typeur.Abs ("y", Typeur.Add (Typeur.Var "y", Typeur.Var "x")), Typeur.N 10))
let ex_liste_ab : Typeur.pterm = Typeur.ListP(Typeur.Vide)
let ex_id_y : Typeur.pterm = Typeur.Abs ("y", Typeur.Var "x") 
let ex_let : Typeur.pterm =  Typeur.Let("x", (N 3), ex_id_y)
let ex_ref : Typeur.pterm =Typeur.Ref ((N 3))
let ex_ref_2 : Typeur.pterm = Typeur.Ref(ex_listP2)
let ex_ref_3 : Typeur.pterm = Typeur.Let("x",Typeur.Ref((N 3)), Typeur.Abs ("y",Typeur.DeRef(Typeur.Var "x")))
let ex_deref : Typeur.pterm =Typeur.DeRef(ex_ref)
let ex_deref_2 : Typeur.pterm =Typeur.DeRef(ex_ref_2)   (* on ne peut pas dref un type il doit etre forcement, on doit forcement dref un type ref *)
let ex_deref_3 : Typeur.pterm =Typeur.DeRef(ex_ref_3)
let ex_ref2 : Typeur.pterm = Typeur.Let("x",Typeur.DeRef((Var "x")), Typeur.DeRef((Var "y")))
let ex_assigne : Typeur.pterm =Typeur.Abs("x", Typeur.Assign((Typeur.Var("x"), (Typeur.Add(Typeur.DeRef(Typeur.Var("x")), Typeur.N 3)))))
let ex_assigne2: Typeur.pterm = Typeur.Let("f", ex_assigne, Typeur.App((Typeur.Var "f"), Typeur.Ref(Typeur.N 2)))
let ex_assigne3: Typeur.pterm = Typeur.App(ex_assigne, Typeur.Ref(Typeur.N 2))
let abs_for_rec = Typeur.Abs("n", Typeur.Izte(Typeur.Var "n", Typeur.N 1, Typeur.Sou(Typeur.App(Typeur.Var "fact", Typeur.Sou(Typeur.Var "n", Typeur.N 1)), Typeur.N 1)))
let abs_for_rec_bis = Typeur.Abs("n", Typeur.Izte(Typeur.Var "n", ex_listP1, Typeur.Sou(Typeur.App(Typeur.Var "fact", Typeur.Sou(Typeur.Var "n", Typeur.N 1)), Typeur.N 1)))
let factorial = Typeur.Pfix("fact", abs_for_rec,Typeur.N 4)
let factorial_bis = Typeur.Pfix("fact", abs_for_rec_bis,Typeur.N 4)


let main () =
  (**
  print_endline "======================";
  print_endline inf_ex_nat3;
  print_endline "======================";
  print_endline ex_Tail_string;
  print_endline "======================";
  print_endline ex_hd_string;
  print_endline "======================";
  print_endline ex_zero_string;*)
  (*print_endline ("term "^Typeur.print_term ex_omega);*)
  (*print_endline("reduction"^Typeur.print_term (Typeur.reduction  ex_omega));*)
  print_endline "======================";
  print_endline (Typeur.print_term ex_id );
  print_endline (Typeur.inference ex_id );
  print_endline (Typeur.print_term convertie_bis);
  print_endline "======================";
  print_endline (Typeur.print_term chat_exemple);
  print_endline (Typeur.print_term exemeple_reduction );
  print_endline "======================";
  print_endline (Typeur.print_term ex_tail);
  print_endline (Typeur.inference ex_tail);
  prerr_endline (Typeur.print_term ex_tail_reduce);
  print_endline "======================";
  print_endline (Typeur.print_term ex_ifz1); 
  print_endline (Typeur.inference ex_ifz1); 
  print_endline (Typeur.print_term ex_ifz_reduce);
  print_endline "======================";
  print_endline (Typeur.print_term ex_ife); 
  print_endline (Typeur.inference ex_ife); 
  print_endline "======================";
  print_endline (Typeur.print_term ex_let); 
  print_endline (Typeur.inference ex_let); 
  print_endline (Typeur.print_term  (Typeur.reduction ex_let [] []));
  print_endline "======================";
  print_endline (Typeur.print_term ex_let_ab);
  print_endline (Typeur.inference ex_let_ab );
  print_endline (Typeur.print_term (Typeur.reduction ex_let_ab [] []));

  (*print_endline (Typeur.print_term ex_omega);*)
  (*print_endline((Typeur.inference ex_omega));   Boucle infinie OK*)
  print_endline "======================";
  print_endline (Typeur.print_term ex_id);
  print_endline "======================";
  print_endline (Typeur.print_term ex_s);
  print_endline inf_ex_s;
  print_endline "======================";
  print_endline (Typeur.print_term ex_nat1);
  print_endline inf_ex_nat1;
  print_endline "======================";
  print_endline (Typeur.print_term ex_listP1);
  print_endline (Typeur.inference ex_listP1);
  print_endline "======================";
  print_endline (Typeur.print_term ex_listP2);
  print_endline (Typeur.inference ex_listP2);
  print_endline "======================";
  print_endline (Typeur.print_term ex_listP3);
  print_endline (Typeur.inference ex_listP3);
  print_endline "======================";
  print_endline (Typeur.print_term ex_listP4);
  print_endline (Typeur.inference ex_listP4);
  print_endline "======================";
  print_endline (Typeur.print_term ex_listP5);
  print_endline (Typeur.inference ex_listP5);
  print_endline "======================";
  print_endline (Typeur.print_term ex_listP6);
  print_endline (Typeur.inference ex_listP6);
  print_endline "======================";
  print_endline (Typeur.print_term ex_hd);
  prerr_endline (Typeur.print_term ex_hd_reduced);
  print_endline "======================";
  print_endline (Typeur.print_term ex_hd);
  print_endline (Typeur.inference ex_hd);
  print_endline "======================";
  print_endline (Typeur.print_term ex_tail);
  print_endline (Typeur.inference  ex_tail);
  print_endline "======================";
  print_endline(Typeur.print_term ex_ifz1);
  print_endline(Typeur.inference ex_ifz1);
  print_endline(Typeur.print_term (Typeur.reduction ex_ifz1 [] []));
  print_endline "======================";
  print_endline(Typeur.print_term ex_ifz3);
  print_endline(Typeur.inference ex_ifz3);
  print_endline "======================";
  print_endline(Typeur.print_term  ex_let);
  print_endline(Typeur.inference ex_let);
  print_endline "======================";
  print_endline(Typeur.print_term ex_ref);
  print_endline(Typeur.print_term (Typeur.reduction ex_ref [] []));
  print_endline  (Typeur.inference ex_ref);
  print_endline "======================";
  print_endline(Typeur.print_term ex_ref_2);
  print_endline(Typeur.print_term (Typeur.reduction ex_ref_2 [] []));
  print_endline  (Typeur.inference ex_ref_2);
  print_endline "======================";
  print_endline(Typeur.print_term ex_ref_3);
  print_endline(Typeur.print_term (Typeur.reduction ex_ref_3 [] []));
  print_endline  (Typeur.inference ex_ref_3);
  print_endline "======================";
  print_endline(Typeur.print_term ex_deref);
  print_endline(Typeur.print_term (Typeur.reduction ex_deref [] []));
  print_endline (Typeur.inference ex_deref);
  print_endline "======================";
  print_endline(Typeur.print_term ex_deref_2);
  print_endline(Typeur.print_term (Typeur.reduction ex_deref_2 [] []));
  print_endline (Typeur.inference ex_deref_2);
  print_endline "======================";
  print_endline(Typeur.print_term ex_assigne);
  (**print_endline(Typeur.print_term (Typeur.reduction ex_assigne [])) ;**)
  print_endline(Typeur.inference ex_assigne);
  print_endline "======================";
  print_endline(Typeur.print_term ex_assigne3);
  print_endline(Typeur.print_term  (Typeur.reduction ex_assigne3 [] [])) ;
  print_endline(Typeur.inference ex_assigne3);
  print_endline "======================";
  print_endline(Typeur.print_term ex_assigne2);
  (**print_endline(Typeur.print_term  (Typeur.reduction ex_assigne [])) ;**)
  print_endline(Typeur.inference ex_assigne2);
  print_endline "======================";
  print_endline(Typeur.print_term factorial);
  print_endline(Typeur.print_term  (Typeur.reduction factorial [] []));
  print_endline(Typeur.inference   factorial );
  print_endline "======================";
  print_endline(Typeur.print_term factorial_bis);
  print_endline(Typeur.inference   factorial_bis )


 
 let _ = main ()

 

(* si reduce alors si non app alors on ne fait rient pas de reduction sinon (si App) on fait substitution on ne renvoie que la partie droite du App donc le body du app *)
(** Si l'element n'est pas bien typé ===> non typable *)