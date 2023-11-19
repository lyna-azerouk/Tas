(* zipper d'une liste d'équations *)
type equa_zip = equa * equa  (*(((ptype * ptype)list *  (ptype * ptype)list))===> ([(type1, type2)], [(type3, type4)]* )*) 
  
(* rembobine le zipper *)
let rec rembobine (e : equa_zip) =
  match e with
    ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2) (* ([(type1, type2), .....] [(type3, type4)] ===>([...], [(type1, type2)::(type3, type4)])*)

(* remplace unee variable par un type dans un zipper d'équations *)
let substitue_type_zip (e : equa_zip) (v : string) (t0 : ptype) : equa_zip =
  match e with
    (e1, e2) -> (substitue_type_partout e1 v t0, substitue_type_partout e2 v t0)

(* trouve un type associé à une variable dans un zipper d'équation *)
let rec trouve_but (e : equa_zip) (but : string) = 
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::_) when v = but -> t
  | (_, (t, Var v)::_) when v = but -> t 
  | (e1, c::e2) -> trouve_but (c::e1, e2) but 
  
  
let substituion_forall var ty =
  let new_ty = ref ty in 
      List.iter (function f -> let nv = Var(nouvelle_var()) in new_ty:= substitue_type ty f nv) var; !new_ty
;;

(* résout un système d'équations *)  (*(((ptype * ptype)list *  (ptype * ptype)list))===> ([(type1, type2)], [(type3, type4)]* )*) 
let rec unification (e : equa_zip) (but : string) : ptype = 
  match e with 
    (* on a passé toutes les équations : succes *)
    (_, []) -> (try trouve_but (rembobine e) but with VarPasTrouve -> raise (Echec_unif "but pas trouvé"))
    (* equation avec but : on passe *)
  | (e1, (Var v1, t2)::e2) when v1 = but ->  unification ((Var v1, t2)::e1, e2) but
    (* deux variables : remplacer l'une par l'autre *)
  | (e1, (Var v1, Var v2)::e2) ->  unification (substitue_type_zip (rembobine (e1,e2)) v2 (Var v1)) but
    (* une variable à gauche : vérification d'occurence puis remplacement *)
  | (e1, (Var v1, t2)::e2) ->  if appartient_type v1 t2 then raise (Echec_unif ("occurence de "^ v1 ^" dans "^(print_type t2))) else  unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) but
    (* une variable à droite : vérification d'occurence puis remplacement *)
  | (e1, (t1, Var v2)::e2) ->  if appartient_type v2 t1 then raise (Echec_unif ("occurence de "^ v2 ^" dans " ^(print_type t1))) else  unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) but 
    (* types fleche des deux cotes : on decompose  *)
  | (e1, (Arr (t1,t2), Arr (t3, t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) but 
    (* types fleche à gauche pas à droite : echec  *)
  | (e1, (Arr (_,_), t3)::e2) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))     
    (* types fleche à droite pas à gauche : echec  *)
  | (e1, (t3, Arr (_,_))::e2) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))     
    (* types nat des deux cotes : on passe *)
  | (e1, (Nat, Nat)::e2) -> unification (e1, e2) but 
    (* types nat à gauche pas à droite : échec *)
  | (e1, (Nat, t3)::e2) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))     
    (* types à droite pas à gauche : échec *)
  | (e1, (t3, Nat)::e2) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))   
  (*Listes *)
  | (e1,(Tliste t1, Tliste t2)::e2) -> unification(e1,(t1,t2)::e2) but (* Aboubakar Diawara *)
  (*3.2.1 Forall *)
  | (e1,((Forall(set1, t1), Forall(set2, t2))::e2)) -> unification(e1,(substituion_forall set1 t1, substituion_forall set2 t2)::e2) but 
  | (e1,((t3, Forall(set2, t2))::e2)) -> unification(e1,((substituion_forall set2 t2),t3)::e2) but 
  | (e1,((Forall(set1, t1), t3)::e2)) -> unification(e1,(t3, (substituion_forall set1 t1))::e2) but 
  (*4.2.1 Reference *)
  | (e1,(RefT t1,RefT t2)::e2) -> unification (e1,(t1,t2)::e2) but
  | (e1,(UnitT ,UnitT )::e2) -> unification (e1,e2) but

(* enchaine generation d'equation et unification *)                                   
let inference (t : pterm) : string =
  let e : equa_zip = ([], genere_equa t (Var "but") []) in
  try (let res = unification e "but" in
       (print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla                   
end
