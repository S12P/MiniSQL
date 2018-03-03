
let appartient2 x l1 = match l1 with
  |t::q -> exists(fun key elmt -> x=elmt, t)

let appartient t1 l1 = match l1 with (*t1 dic*)
  |t::q -> equal( fun a b -> a=b, t1, t)

let union l1 l2 = (*l1 et l2 liste de dictionnaire*)
  let rec inter l1 l2 l = match l2 with
    | [] -> l
    | t::q when (appartient t l1) -> inter l1 q t::l
    | t::q -> inter l1 q l
  in inter l1 l2 []

let minus l1 l2 =
  let rec inter l1 l2 l = match l2 with
    | [] -> l
    | t::q when not (appartient t l1) -> inter l1 q t::l
    | t::q -> inter l1 q l
  in inter l1 l2 []
