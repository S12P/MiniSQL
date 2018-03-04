

let appartient t1 l1 = match l1 with (*fonction utile pour union et minus avec t1 un dictionnaire et l1 une liste de dictionnaire*)
  |t::q -> equal( fun a b -> a=b, t1, t)

let union l1 l2 = (*fonction qui permet de faire l'union avec l1 et l2 liste de dictionnaire*)
  let rec inter l1 l2 l = match l2 with
    | [] -> l
    | t::q when (appartient t l1) -> inter l1 q t::l
    | t::q -> inter l1 q l
  in inter l1 l2 []

let minus l1 l2 = (*fonction qui permet de faire minus avec l1 et l2 liste de dictionnaire*)
  let rec inter l1 l2 l = match l2 with
    | [] -> l
    | t::q when not (appartient t l1) -> inter l1 q t::l
    | t::q -> inter l1 q l
  in inter l1 l2 []

  let appartient2 x l1 = match l1 with (*fonction utile pour ?*)
    |t::q -> exists(fun key elmt -> x=elmt, t)
