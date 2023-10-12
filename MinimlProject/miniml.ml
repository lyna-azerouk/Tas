module StringMap = Map.Make(String)

type 'a liste = Vide | Cons of 'a * 'a liste

(* Termes *)
type pterm = Var of string
 | App of pterm * pterm 
 | Abs of string * pterm 
 | N of int 
 | Add of pterm * pterm 
 | Sou of pterm * pterm 
 | ListP of pterm liste
 | Hd of  pterm 
 | Tail of  pterm  
 | Izte of pterm  * pterm * pterm 
 | Iete of pterm * pterm * pterm 

(* Types *) 
type ptype = Var of string | Arr of ptype * ptype | Nat | Tliste of ptype(* Arr le type des fonctions Arr (Nat, Var "int") pourrait représenter le type d'une fonction qui prend un argument de type Nat et retourne un résultat de type Var "int".*)
(* Environnements de typage *) 
type env = (string * ptype) list  (* liste de type de chaque variable  (var , ptype)*)
(* Listes d'équations *) 
type equa = (ptype * ptype) list (* liste de couples de types*)
(* pretty printer de termes*)     
let rec print_term_liste t =
  let rec aux = function  
        Vide ->"nil"
        |Cons (t1, t2)-> (print_term t1)^", "^(aux t2) in 
        "["^ aux  t ^"]"
       
and print_term (t : pterm) : string =
  match t with
    Var x -> x
    | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"   
    | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")" 
    | N n -> string_of_int n
    | Add (t1, t2) -> "(" ^ (print_term t1) ^" + "^ (print_term t2) ^ ")"
    | Sou(t1,t2) ->  "("^ (print_term t1)^ " - " ^(print_term t2) ^")"
    | Hd t1 ->"hd(" ^(print_term t1) ^")"
    | Tail t1 -> "tail(" ^(print_term t1) ^")"
    | Izte (condition, t1, t2) -> "if "^(print_term condition)^ "then  " ^(print_term t1)^"else " ^(print_term t2)
    | Iete (condition, t1, t2) ->  "if "^(print_term condition)^ "then  " ^(print_term t1)^"else " ^(print_term t2)
    | ListP  ti -> print_term_liste ti
  

    
(* pretty printer de types*)                   
let rec print_type (t : ptype) : string =
  match t with
    Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^" -> "^ (print_type t2) ^")"
  | Nat -> "Nat"
  | Tliste l -> "[" ^ print_type l ^ "]"


(* générateur de noms frais de variables de types *)
let compteur_var : int ref = ref 0                    

let nouvelle_var () : string = compteur_var := !compteur_var + 1; 
  "T"^(string_of_int !compteur_var)


exception VarPasTrouve

(* cherche le type d'une variable dans un environnement *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::q when v1 = v -> t1
  | (_, _):: q -> (cherche_type v q) 

(* vérificateur d'occurence de variables *)  
let rec appartient_type (v : string) (t : ptype) : bool =
  match t with
    Var v1 when v1 = v -> true
  | Arr (t1, t2) -> (appartient_type v t1) || (appartient_type v t2) 
  |Tliste l ->  appartient_type  v  l
  | _ -> false

(* remplace une variable par un type dans type *)
let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
  match t with
    Var v1 when v1 = v -> t0
  | Var v2 -> Var v2
  | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0) 
  | Nat -> Nat 
  |Tliste l -> Tliste(substitue_type l v t0)

(* remplace une variable par un type dans une liste d'équations*)
let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

(*alpha converstion*)
let rec alpha_conv(l : pterm) (orig: string) (new_var :string ): pterm =
  match l with
    Var variable_to_replace when variable_to_replace = orig -> (Var new_var)
  | N value->(N value)
  | App(t1, t2) ->App(alpha_conv t1 orig new_var, alpha_conv t2 orig new_var)
  | Abs(s, t2)  when s = orig -> Abs(new_var, alpha_conv t2 orig new_var)
  | Add(t1, t2) ->Add(alpha_conv t1 orig new_var, alpha_conv t2 orig new_var)
  | Sou(t1, t2) ->Sou(alpha_conv t1 orig new_var, alpha_conv t2 orig new_var)
  | ListP l ->match l with 
             |Vide ->ListP(Vide)
             |Cons (l1, ls)-> ListP(Cons((alpha_conv l1 orig new_var), alpha_conv_list ls  orig new_var))

  and alpha_conv_list (lst : pterm liste) (orig : string) (new_var : string) : pterm liste =
             match lst with
             | Vide -> Vide
             | Cons (l1, ls) ->
               let new_l1 = alpha_conv l1 orig new_var in
               let new_ls = alpha_conv_list ls orig new_var in
               Cons (new_l1, new_ls)

(*alpha converstion Bis*)
let rec alpha_conv_bis(l : pterm) acc: pterm =
  match l with
  | Var variable_to_replace ->
    (match acc with
    | [] ->
        let nv = nouvelle_var () in
        Var nv
    | (var, new_var) :: rest ->
        if var = variable_to_replace then
          Var new_var
        else
          Var (nouvelle_var ()))  (* Using nouvelle_var() to generate a new variable *)
  | N value -> N value
  | Abs(s, t2) -> 
    (match acc with
    |[] -> let nv= nouvelle_var() in 
           (Abs(nv, alpha_conv_bis t2 [(s, nv)]));
    |(s1, value)::rest when s1 =s ->Abs(value, alpha_conv_bis t2 acc)
    |_::rest -> (alpha_conv_bis  (Abs(s,t2))  rest ))
  | App(t1, t2) ->App(alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
  | Add(t1, t2) ->Add(alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
  | Sou(t1, t2) ->Sou(alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
  (*| ListP l ->match l with 
             |Vide ->ListP(Vide)
             |Cons (l1, ls)-> ListP(Cons((alpha_conv l1 acc), alpha_conv_list ls acc))
  and alpha_conv_list (lst : pterm liste) acc : pterm liste =
             match lst with
             | Vide -> Vide
             | Cons (l1, ls) ->
               let new_l1 = alpha_conv l1 orig new_var in
               let new_ls = alpha_conv_list ls orig new_var in
               Cons (new_l1, new_ls)

*)

               
(* genere des equations de typage à partir d'un terme *)  
(* ty pour type attendue *)
let map_liste (l : pterm liste) ty e f =
  let rec aux_map l ty e f =
    match l with
    | Vide -> let nhv : string = nouvelle_var() in
              [(ty, Tliste(Var nhv))]
    | Cons (l, ls) -> let nhv : string = nouvelle_var() in
      (ty, Tliste(Var nhv))::(f l (Var nhv) e)@(aux_map ls (Tliste(Var nhv)) e f)
            in aux_map l ty e f


let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with 
    Var v -> let tv : ptype = cherche_type v e in [(ty, tv)]  (*tv est de type ptype*) (** La fonction renvoie une liste contenant une seule équation de type qui indique que  le type attendu ty est équivalent au type de la variable v. *)
  | App (t1, t2) -> let nv : string = nouvelle_var () in (* why nv*)
      let eq1 : equa = genere_equa t1 (Arr (Var nv, ty)) e in  (* le type  attendue de de t1 est un Arr(Var nv), ty) *)
      let eq2 : equa = genere_equa t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t) -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      (ty, Arr (Var nv1, Var nv2))::(genere_equa t (Var nv2) ((x, Var nv1)::e))  
  | N _ -> [(ty, Nat)]
  | Add (t1, t2) -> let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat)::(eq1 @ eq2)   (*le but etant d'associer de construire une liste d'equations, on calcule l'equation de  t1 et t2 puis concatenation avec (ty, Nat)*)
  | Sou (t1, t2) -> let eq1: equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in 
      (ty, Nat)::(eq1 @ eq2)
  |Tail t1 -> let nhv : string = nouvelle_var() in 
    let eq : equa=genere_equa t1 (Tliste (Var nhv)) e  in 
    (ty, (Tliste (Var nhv)))::eq
  |ListP l-> map_liste  l ty e genere_equa
  |Hd t1 -> let nhv : string = nouvelle_var() in 
      let eq : equa=genere_equa t1  (Tliste(Var nhv)) e in 
        (ty , (Var nhv))::eq
  |Izte (condition, t1, t2) ->let nvh1: string =nouvelle_var() in 
      let nvh2: string =nouvelle_var() in 
        let cond: equa =genere_equa condition Nat e in 
          let eq1: equa =genere_equa t1 ty e in   (* what is the type expected of t1*)
          let eq2: equa =genere_equa t2 ty e in 
            (cond@eq1@eq2)

exception Echec_unif of string      

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
  |(e1,(Tliste t1, Tliste t2)::e2) -> unification(e1,(t1,t2)::e2) but (* Aboubakar*)
                                       
(* enchaine generation d'equation et unification *)                                   
let inference (t : pterm) : string =
  let e : equa_zip = ([], genere_equa t (Var "but") []) in
  try (let res = unification e "but" in
       (print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla
                         
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
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : pterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = inference ex_nat3
let ex_sous : pterm =    Sou((N 3), (N 3))
let ex_sous_string : string =  inference ex_sous
let ex_listP1 : pterm = ListP (Cons( (N 3), Cons (ex_sous, Vide)))
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
let convertie_bis : pterm = alpha_conv_bis ex_nat1  []
let convertie_string : string = inference convertie_bis 


let exemple = alpha_conv_bis ex_s []
let exemple_string : string = print_term exemple


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
  print_endline (print_term ex_s);
  print_endline (print_term exemple)

  
 
 let _ = main ()

 

(* si reduce alors si non app alors on ne fait rient pas de reduction sinon (si APP) on fait substitution on ne renvoie que la partie droite du app donc le body du app *)