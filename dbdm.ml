open DataType

(*on va avoir un dictionnaire de liste de dictionnaire ie un dictionnaire de tt les tables*)
let avoir_table table base_de_donnee : string StringMap.t list = StringMap.find table base_de_donnee


let appartient t1 l1 = match l1 with (*fonction utile pour union et minus avec t1 un dictionnaire et l1 une liste de dictionnaire*)
  |t::q -> StringMap.equal (fun a b -> a=b) t1 t

let union l1 l2 = (*fonction qui permet de faire l'union avec l1 et l2 liste de dictionnaire*)
  let rec inter l1 l2 l = match l2 with
    | [] -> l
    | t::q when (appartient t l1) -> inter l1 q l
    | t::q -> inter l1 q (t::l)
  in inter l1 l2 []

let minus l1 l2 = (*fonction qui permet de faire minus avec l1 et l2 liste de dictionnaire*)
  let rec inter l1 l2 l = match l2 with
    | [] -> l
    | t::q when (appartient t l1) -> inter l1 q l
    | t::q -> inter l1 q (t::l)
  in inter l1 l2 []

let appartient2 x l1 = match l1 with (*fonction utile pour le In dans test_cond*)
  |t::q -> StringMap.exists (fun key elmt -> x=elmt) t


let rec test_cond base_de_donnee nbligne cond : bool = (*permet de tester la condition du where*)
    match cond with
      | And (c1, c2) -> (test_cond base_de_donnee nbligne c1 ) && (test_cond base_de_donnee nbligne c2 )
      | Or(c1, c2) -> (test_cond base_de_donnee nbligne c1 ) || (test_cond base_de_donnee nbligne c2 )
      | Rel(s1, Eq, s2) -> (match s1 with
                          | ID (table, colonne) -> let table = (avoir_table table base_de_donnee) in StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          )
                          =
                          (match s2 with
                            | ID (table, colonne) -> let table = (avoir_table table base_de_donnee) in StringMap.find colonne (List.nth table nbligne)  (* valeur dans la base de données *)
                            | String(x) -> x
                          )
      | Rel(s1, Lt, s2) -> ((*try*) let x = (match s1 with
                          | ID (table, colonne) -> let table = (avoir_table table base_de_donnee) in StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          ) in int_of_string x(*) with _ -> s1)*))
                          <
                          ((*try*) let x = (match s2 with
                          | ID (table, colonne) -> let table = (avoir_table table base_de_donnee) in StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          ) in int_of_string x (*with _ -> s2)*))


let where_int col table cond =
  let rec inter1 col table cond l = match table with
    | [] -> l
    | tab::autretable -> let rec inter11 col table cond numligne l = match numligne with
                          | -1 -> inter1 col autretable cond l
                          | n ->  let rec inter2 col table cond numl l = match col with
                            | [] -> inter11 col table cond (numl-1) l
                            | co::autreco -> let rec inter3 co table cond condi numll l = match condi with
                              | true -> l @ (StringMap.find co (List.nth table numll)) ; inter2 autreco table cond numll l
                              | false -> inter2 autreco table cond numll l
                            in inter3 co table cond (test_cond table numl cond) numl l
                        in inter2 col table cond numligne l
                      in inter11 col tab cond ((List.length tab)-1) l
  in inter1 col table cond []


let where col base_de_donnee cond = match col with
  | (a, b)::q -> let table = (avoir_table a base_de_donnee); where_int b table cond


(*let select entete csv requete =
    let rec affiche csv listeaux =
        match csv with
         | [] -> ()
         | t :: q when test_cond t ->affiche q (t :: listeaux)
         | t :: q -> affiche q listeaux


let comp =
    maps d1 d2 (fun d1 d2 -> fun i -> d1.i == d2.i)*)
