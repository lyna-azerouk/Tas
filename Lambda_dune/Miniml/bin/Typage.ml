(* genere des equations de typage à partir d'un terme *)  
(* ty pour type attendue *)
let map_liste_gen_equa (l : pterm liste) ty e f =
  let rec aux_map l ty e f =
    match l with
    | Vide -> let nhv : string = nouvelle_var() in
              [(ty, Tliste(Var nhv))]
    | Cons (l, ls) -> let nhv : string = nouvelle_var() in
      (ty, Tliste(Var nhv))::(f l (Var nhv) e)@(aux_map ls (Tliste(Var nhv)) e f)
            in aux_map l ty e f
   
let rec generalize_types (env : env) (equations) : env =
match equations with
| [] -> env
| (t1, t2) :: rest ->
    let generalized_env = generalize_type env t1 t2 in
    generalize_types generalized_env rest

and generalize_type (env : env) (t1 : ptype) (t2 : ptype) : env =
match (t1, t2) with
| Var v1, Var v2 when v1 = v2 -> env
| Var v, _ -> (v, t2) :: env
| _, Var v -> (v, t1) :: env
| _ -> env
(* return all type variables of t*)

let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with 
    Var v -> let tv : ptype = cherche_type v e in [(ty, tv)]  (** La fonction renvoie une liste contenant une seule équation de type qui indique que  le type attendu ty est équivalent au type de la variable v. *)
  | App (t1, t2) -> let nv : string = nouvelle_var () in 
      let eq1 : equa = genere_equa t1 (Arr (Var nv, ty)) e in  (* le type  attendue de de t1 est un Arr(Var nv), ty) *)
      let eq2 : equa = genere_equa t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t) -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      (ty, Arr (Forall ([], Var nv1),  Var nv2))::(genere_equa t (Var nv2) ((x,  Var nv1)::e))  
  | N _ -> [(ty, Nat)]
  | Add (t1, t2) -> let eq1 : equa = genere_equa t1 Nat e and eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat)::(eq1 @ eq2) 
  | Sou (t1, t2) -> let eq1: equa = genere_equa t1 Nat e and eq2 : equa = genere_equa t2 Nat e in 
      (ty, Nat)::(eq1 @ eq2)
  |Tail t1 -> let nhv : string = nouvelle_var() in 
    let eq : equa=genere_equa t1 (Tliste (Var nhv)) e  in 
    (ty, (Tliste (Var nhv)))::eq

  |ListP l-> map_liste_gen_equa  l ty e genere_equa
  
  |Hd t1 -> let nhv : string = nouvelle_var() in 
      let eq : equa=genere_equa t1  (Tliste(Var nhv)) e in 
        (ty , (Var nhv))::eq
  |Izte (condition, t1, t2) ->(let cond: equa =genere_equa condition Nat e in 
   let varh :string = nouvelle_var() in
     let eq1: equa =genere_equa t1 (Var varh)  e and eq2: equa =genere_equa t2 (Var varh) e in 
                ((ty, (Arr(Nat, (Var varh))))::cond@ (eq1@eq2)))
  |Iete (condition, t1, t2) ->(let nvh1: string =nouvelle_var() in
    let cond: equa = genere_equa condition (Tliste(Var (nouvelle_var()))) e in  
    let eq1 : equa =genere_equa t1 (Var nvh1) e and eq2: equa =genere_equa t2 (Var nvh1) e in 
        ((ty, (Arr(Tliste((Var nvh1)), (Var nvh1))))::(cond@eq1@eq2)))

  |Pfix (s, t1,t2) -> (let nv: string  =  nouvelle_var() in 
                let eq1 : equa = genere_equa t1 ty ((s,(Var nv))::e)  in    (*To so :use forall for généralisation*)
                let   new_env = generalize_types ((s,(Var nv))::e)  eq1 in
                let eq2 : equa =  genere_equa t2 ty  (new_env@e) in 
                (ty, Var(nv))::(eq1@eq2))

  |Let (s, e1, e2) -> (let nvh :  string = nouvelle_var() in
        let var_ptype : ptype = (Var  nvh) in
        let nvh2 : string = nouvelle_var() in
          let eq1 : equa = genere_equa e1 (Var nvh) e  in 
          let   new_env = generalize_types  ((s,(Var nvh))::e)  eq1 in
              let eq2 : equa =  genere_equa e2 ty  (new_env@e) in 
                  (ty, var_ptype)::(eq1@eq2)) 
  |Ref t1-> (let nvh : ptype = (Var (nouvelle_var())) in 
              let  var_type: ptype = (RefT nvh) and eq1 : equa = genere_equa t1 nvh e in 
                (ty,var_type)::eq1)
  |DeRef t1 -> let nvh :string = nouvelle_var() in 
                let eq1: equa= (genere_equa t1 (RefT (Var nvh))  e) in 
                  (ty, (Var nvh))::eq1
  |Assign (t1, t2) -> let nv: string = nouvelle_var() in (let eq1: equa = (genere_equa t1 (RefT (Var nv)) e) and  eq2: equa = (genere_equa t2 (Var nv ) e ) in 
                  (ty, UnitT)::(eq1@eq2))