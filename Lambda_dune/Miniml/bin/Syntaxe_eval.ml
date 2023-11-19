module Typeur = struct
  module StringSet = Set.Make(String)
  type 'a liste = Vide | Cons of 'a * 'a liste
  exception Echec_unif of string      
  exception VarPasTrouve
  exception Echec_reduction      
  
  (****************************************** I. Termes ******************************************)
  (* Termes *)
  type pterm = Var of string
   | App of pterm * pterm  (*Représente l'application d'une fonction à un argument*)
   | Abs of string * pterm (*la création d'une fonction lambda*)
   | N of int 
   | Add of pterm * pterm 
   | Sou of pterm * pterm 
   | ListP of pterm liste
   | Hd of pterm 
   | Tail of  pterm  
   | Izte of pterm  * pterm * pterm 
   | Iete of pterm * pterm * pterm 
   | Pfix  of string * pterm *pterm
   | Let of string * pterm * pterm 
   (* 4.1.1 *)
   | Ref of  pterm 
   | DeRef of pterm 
   | Assign of  pterm * pterm
   | Unit 
  
  (* pretty printer de termes*)     
  let rec print_term_liste t =
    let rec aux = function  
          Vide ->"nil"
          |Cons (t1, t2)-> (print_term t1)^", "^(aux t2) in 
          "["^ aux  t ^"]"
         
  and print_term (t : pterm) : string =
    match t with
      (* 2.1.2 *)     
      Var x -> x
      | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"   
      | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")" 
      | N n -> string_of_int n
      | Add (t1, t2) -> "(" ^ (print_term t1) ^" + "^ (print_term t2) ^ ")"
      (* 3.1.2 *)  
      | Sou(t1,t2) ->  "("^ (print_term t1)^ " - " ^(print_term t2) ^")"
      | Hd t1 ->"hd(" ^(print_term t1) ^")"
      | Tail t1 -> "tail(" ^(print_term t1) ^")"
      | Izte (condition, t1, t2) -> "if "^(print_term condition)^ " then  " ^(print_term t1)^" else " ^(print_term t2)
      | Iete (condition, t1, t2) ->  "if "^(print_term condition)^ " then  " ^(print_term t1)^" else " ^(print_term t2)
      | ListP  ti -> print_term_liste ti
      | Let (s, t1, t2) ->  "let "^s^" = "^(print_term t1)^" in "^(print_term t2)
      | Pfix (s , t1,t2)  -> "let rec "^s^ " ="^(print_term t1) ^"in " ^s^" " ^(print_term t2)
      (* 4.1.2 *) 
      | Ref e ->  "(ref " ^(print_term e) ^")"
      | DeRef e -> "!" ^ (print_term e)
      | Assign (e1, e2) ->  (print_term e1) ^ ":=" ^ (print_term e2)
      | Unit -> "()"
  
  (****************************************** I. Types ******************************************)
  type ptype = Var of string 
    | Arr of ptype * ptype 
    | Nat 
    (* 3.3.1 *)  
    | Tliste of ptype  
    | Forall of (string list) * ptype
    (* 4.3.1 *)
    |UnitT 
    |RefT of ptype 
  
  (* Environnements de typage *) 
  type env = (string * ptype) list  (* liste de type de chaque variable  (var , ptype)*)
  (* Listes d'équations *) 
  type equa = (ptype * ptype) list (* liste de couples de types*)
  
  (* pretty printer de types*)                   
  let rec print_type (t : ptype) : string =
    match t with
      Var x -> x
    | Arr (t1, t2) -> "(" ^ (print_type t1) ^" -> "^ (print_type t2) ^")"
    | Nat -> "Nat"
    | Tliste l -> "[" ^ print_type l ^ "]"
    | Forall (set, t1) -> "Forall " ^(List.fold_left (fun elem acc -> acc ^ elem) "" set )^ " "^(print_type t1) 
    |UnitT -> "Unit"
    |RefT e -> "(ref " ^(print_type e) ^")"
  
  (* générateur de noms frais de variables de types *)
  let compteur_var : int ref = ref 0                    
  
  let nouvelle_var () : string = compteur_var := !compteur_var + 1; 
    "T"^(string_of_int !compteur_var)
  
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
    |Tliste l -> appartient_type  v  l
    |RefT r ->  appartient_type v  r 
    | Forall (set, t1) -> List.exists (fun element -> appartient_type element t1) set
    | _ -> false
  
  (* remplace une variable par un type dans type *)
  let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
    match t with
      Var v1 when v1 = v -> t0
    | Var v2 -> Var v2
    | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0) 
    | Nat -> Nat 
    |Tliste l -> Tliste(substitue_type l v t0)
    |RefT t1 -> RefT(substitue_type t1 v t0)
    |Forall (set, t1) when (List.mem v set) -> Forall (set,(substitue_type t1 v t0))
    |Forall (set, t1) -> t1
    |UnitT -> UnitT
    
  (* remplace une variable par un type dans une liste d'équations*)
  let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
    List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e
  
  (*Alpha converstion Bis*)
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
            alpha_conv_bis (Var variable_to_replace) rest)
    | N value -> N value
    | Abs(s, t2) -> let nv= nouvelle_var() in 
             (Abs(nv, alpha_conv_bis t2 ((s, nv)::acc)));
    | App(t1, t2) ->App(alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
    | Add(t1, t2) ->Add(alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
    | Sou(t1, t2) ->Sou(alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
    | Hd t1 ->Hd(alpha_conv_bis t1 acc)
    | Tail t1 -> Hd(alpha_conv_bis t1 acc)
    | Izte (cond, t1, t2)-> Izte(alpha_conv_bis cond acc, alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
    | Iete (cond, t1, t2)->Izte(alpha_conv_bis cond acc, alpha_conv_bis t1 acc, alpha_conv_bis t2 acc)
    | Let (s, t1, t2) ->let nv: string =nouvelle_var() in 
                          (Let(nv, alpha_conv_bis t1 ((s, nv)::acc), alpha_conv_bis t2 ((s, nv)::acc)))                        
    |Ref  e -> Ref (alpha_conv_bis e acc)
    |DeRef e -> DeRef (alpha_conv_bis e acc)
    |Pfix (s, t1, t2) -> Pfix (s, alpha_conv_bis t1 acc, alpha_conv_bis t2 acc )
    |Unit -> Unit
    | ListP l ->match l with 
               |Vide ->ListP(Vide)
               |Cons (l1, ls)-> ListP(Cons((alpha_conv_bis l1 acc), alpha_conv_list ls acc))
    
  and alpha_conv_list (lst : pterm liste) acc : pterm liste =
               match lst with
               | Vide -> Vide
               | Cons (l1, ls) ->
                 let new_l1 = alpha_conv_bis l1 acc in
                 let new_ls = alpha_conv_list ls acc in
                 Cons (new_l1, new_ls)
  
  (****************************************** III. Evaluation ***************************************************************************************)
  (* fonction   map sur les liste *)
  let map_list (lst :pterm liste) acc  f = 
    let rec map_list_aux l f  =
       match l with 
       | Vide -> Vide
       | Cons (t1, ls)->Cons((f t1 acc []), (map_list_aux ls f ))
          in map_list_aux lst f 
  
  let rec find_in_memo (s :string) acc = 
    match acc with 
    | []->print_endline("je uis la ");raise Echec_reduction
    | (variable, value)::tail when  s= variable -> value
    | (variable, value)::tail  ->  find_in_memo s tail
  
  (* Evaluation*)
  let rec reduction (t: pterm)  acc env : pterm=
    match t with 
    App(Abs(s, t1), t2) -> (substitution t1 s t2)  ((s,t2)::acc) env  (* β-reduction  uniqument la suvstiution M'[N/x]*)
    | App (m, n) ->  
        let m' = reduction m acc env  in
        let n' = reduction n acc env  in
        (match m' with 
            |Abs(s, t1) ->App ((substitution t1 s m' acc env ), n')
            |Var s ->  reduction (App((find_in_memo s env), n')) ((s,n')::acc) env  (* cherche en memoire la fonction et on fait un appel recurssive sur *)
            |_ -> App (m', n'))
    | Abs (x, m) -> Abs (x, reduction m acc env )   (*valeur par defaut pour x *)
    | Add(t1, t2) -> let val_1 : pterm = reduction t1 acc env  and  val_2 : pterm = reduction t2 acc  env in 
                      (match (val_1, val_2) with 
                        |((N val1), (N val2)) -> (N(val1 + val2))
                        |_ ->  raise Echec_reduction)
    |N t1 -> (N t1)
    |Var s -> (Var s)
    |Sou(t1, t2) ->  let val_1 : pterm = reduction t1  acc env  and val_2 : pterm = reduction t2 acc env  in 
                      (match (val_1, val_2) with 
                        |((N val1), (N val2)) -> (N(val1 - val2))
                        |_ -> raise Echec_reduction)
    |Hd t1 -> (match t1 with 
                |ListP(Vide)-> ListP(Vide)
                |ListP(Cons(fst,_)) ->reduction fst acc env 
                |_ -> raise Echec_reduction)
  
    |Tail t1 -> (match t1 with 
                |ListP(Vide)-> ListP(Vide)
                |ListP(Cons(_,rest)) -> match rest with 
                                      |Vide -> ListP(Vide)
                                      |Cons(l1,ls) -> ListP(Cons ((reduction l1 acc env ), (map_list ls acc   reduction)));
                |_ -> raise Echec_reduction)
  
    |Izte (cond, t1, t2) -> let cond_reduced : pterm=(reduction cond acc env)in 
                            (match cond_reduced with 
                                | (N 0)-> (reduction t1 acc env)
                                | Var s -> (match find_in_memo  s acc with
                                               |(N 0) -> (reduction t1 acc env )
                                               |_  -> (reduction t2 acc env)  )
                                | _ -> (reduction t2 acc env ))
    |Iete (cond, t1, t2) -> let cond_reduced : pterm=(reduction cond acc env)in 
                            (match cond_reduced with 
                            | ListP(Vide) -> (reduction t1 acc env)
                            | ListP(_) -> (reduction t2 acc env )
                            | _ -> raise Echec_reduction)
    |Pfix(s ,t1,t2) ->  (match t1 with 
                    |Abs(var, corps) ->reduction (App(t1, t2)) acc ((s,t1)::env)
                    |_ -> raise Echec_reduction)
    | ListP t1 -> (match t1 with 
                  |Vide -> ListP(Vide)
                  |Cons (l1, ls) -> ListP(Cons (reduction l1 acc env, map_list ls acc reduction));)
    |Let (s, t1, t2) ->(let reduce_t1 =reduction t1 acc env in 
                  (match reduce_t1 with 
                    |Ref f -> (reduction t2 ((s,f)::acc)) env
                    |Abs(s,n) -> (match t2 with 
                                  |App(val1, val2)->  substitution (Abs(s,n)) s val2 acc env 
                                  |_-> raise Echec_reduction)
                    |_ ->reduction((substitution (reduction t2  ((s, reduce_t1)::acc) env) s reduce_t1) acc env) ((s, reduce_t1)::acc) env) ) 
    |Ref t1 -> let e_reduction:pterm = reduction t1 acc  env in 
                Ref(e_reduction) 
    |DeRef e -> let e_reduction : pterm =(reduction e acc) env  in 
                (match e_reduction with 
                  |Ref e -> e
                  |Var x_reduction->(match find_in_memo  x_reduction acc with
                              |value -> value )
                  |_ ->  raise Echec_reduction)
    |Assign (t1, t2) -> let e2: pterm= (reduction t2 acc env)  in
                       (match t1 with 
                          |Ref p ->   (Assign(t1, t2)) 
                          |Var x -> Assign (t1, t2) 
                          |_ ->  raise Echec_reduction)
    |Unit -> Unit
    | _ -> t
  
  (*Substitution de la variable x par le term "nterm"*)
  and substitution terme x nterm  acc env =
    match terme with
    | Var y when y = x ->   nterm
    | Var y ->  Var y
    | N v -> (N v)
    | App (t1, t2) ->  App (substitution t1 x nterm acc env , substitution t2 x nterm acc env )
    | Abs (y, m) when y <> x ->   Abs (y, substitution m x nterm acc env )   (*raise TimeOut*)
    | Abs (_, _) ->  let z = nouvelle_var () in substitution (alpha_conv_bis terme []) x (alpha_conv_bis (Abs (z, Var z)) []) acc env 
    | Add(t1,t2)->  Add (substitution t1 x nterm acc env , substitution t2 x nterm acc env )
    | Sou (t1,t2) -> Sou (substitution t1 x nterm acc env , substitution t2 x nterm acc env )
    | Ref t1 -> Ref (substitution t1 x nterm acc env )
    | DeRef t1 ->DeRef (substitution t1 x nterm acc env )
    | Assign (t1, t2) ->  (Assign(t1, substitution t2 x nterm acc env ))
    | Pfix (s,t1,t2) -> Pfix (s, substitution t1 x nterm acc env ,t2)
    | Izte (cond, t1, t2) -> reduction (Izte (cond, t1, (substitution t2 x nterm acc env )))   acc env
    | _  ->   terme
  