open DataType

type op = Eq | Lt | Gt

type idstring = ID of (string StringMap.t list) * string (*table colonne*)
      |  String of string

type cond = And of cond * cond
        | Or of cond * cond
        | Rel of idstring * op * idstring
        | In of string * (string StringMap.t list)

type requete = {col: string list; table: string list; cond: cond}


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


let rec test_cond table nbligne cond : bool = (*permet de tester la condition du where*)
    match cond with
      | And (c1, c2) -> (test_cond table nbligne c1 ) && (test_cond table nbligne c2 )
      | Or(c1, c2) -> (test_cond table nbligne c1 ) || (test_cond table nbligne c2 )
      | Rel(s1, Eq, s2) -> (match s1 with
                          | ID (table, colonne) -> StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          )
                          =
                          (match s2 with
                            | ID (table, colonne) -> StringMap.find colonne (List.nth table nbligne)  (* valeur dans la base de données *)
                            | String(x) -> x
                          )
      | Rel(s1, Lt, s2) -> ((*try*) let x = (match s1 with
                          | ID (table, colonne) -> StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          ) in int_of_string x(*) with _ -> s1)*))
                          <
                          ((*try*) let x = (match s2 with
                          | ID (table, colonne) -> StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          ) in int_of_string x (*with _ -> s2)*))
      | Rel(s1, Gt, s2) -> ((*try*) let x = (match s1 with
                          | ID (table, colonne) -> StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          ) in int_of_string x (*with _ -> s1*))
                          >
                          ((*try*) let x = (match s2 with
                          | ID (table, colonne) -> StringMap.find colonne (List.nth table nbligne)
                          | String(x) -> x
                          ) in int_of_string x (*with _ -> s2)*))
      | In (s, l) -> if appartient2 s l then
                          true
                      else false


let where col table cond =
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


(*let select entete csv requete =
    let rec affiche csv listeaux =
        match csv with
         | [] -> ()
         | t :: q when test_cond t ->affiche q (t :: listeaux)
         | t :: q -> affiche q listeaux


let comp =
    maps d1 d2 (fun d1 d2 -> fun i -> d1.i == d2.i)*)
