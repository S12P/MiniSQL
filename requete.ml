open DataType
open Cond


module Requete = struct
	
	
	type t = requete
	
	
	(* Normalise pour enlever les In *)
    let rec normalize_req (req : t) : t =
        (* On analyse une condition et on retourne les tables des in, la condition modifiée, et les conditions auxiliaires *)
        let rec analyse_cond c =
            match c with
            | And(c1, c2) -> let tables1, cond1, condaux1 = analyse_cond c1 in
                             let tables2, cond2, condaux2 = analyse_cond c2 in
                             let condaux = match condaux1, condaux2 with
                                | None, None -> None
                                | None, Some x -> Some x
                                | Some x, None -> Some x
                                | Some x, Some y -> Some (And(x, y))
                             in
                             tables1 @ tables2, And(cond1, cond2), condaux
            | Or(c1, c2) -> let tables1, cond1, condaux1 = analyse_cond c1 in
                            let tables2, cond2, condaux2 = analyse_cond c2 in
                            let condaux = match condaux1, condaux2 with
                                | None, None -> None
                                | None, Some x -> Some x
                                | Some x, None -> Some x
                                | Some x, Some y -> Some (Or(x, y))
                            in
                            tables1 @ tables2, Or(cond1, cond2), condaux
            | Rel(_,_,_) -> [], c, None
            | In(id, table) -> let table = normalize_req table in
                                begin
                                    match table with
                                    | Where({col = c; table = ltable ; cond = cond}) ->
                                            if (List.length c) = 1 then
                                                let c = match List.hd c with 
                                                		Col(CID(x, y)) -> ID(x, y) 
                                                		| Rename(CID(x,y), _) -> ID(x, y) 
                                                		| _ -> failwith "Impossible de traiter la requete avec des fonctions d'agrégations"
                                                in
                                                ltable, Rel(c, Eq, id), Some cond
                                            else
                                                failwith "Error in the query"
                                    | _ -> failwith "I don't how to normalize this query"
                                end
            | NotIn (_, _) -> failwith "Il ne devrait à cette étape plus y avoir de NOT IN. Si c'est le cas c'est que la requête ne peut pas être traîtée."
        in
        match req with
        | Group(req, col) -> Group(normalize_req req, col)
        | Order(req, col) -> Order(normalize_req req, col)
        | Union(r1, r2) -> Union(normalize_req r1, normalize_req r2)
        | Minus(r1, r2) -> Minus(normalize_req r1, normalize_req r2)
        | Where({col = lc; table = ltable ; cond = cond}) ->
                let newtable, newcond, condaux = analyse_cond cond in
                let t =
                    let analyse_col x =
                        match x with
                        | Req(r, name) -> Req(normalize_req r, name)
                        | _ -> x
                    in
                    List.map analyse_col (ltable @ newtable)
                in
                let c = match condaux with
                    | None -> newcond
                    | Some x -> And(newcond, x)
                in
                Where ({ col = lc; table = t; cond = c })








    (* Supprime les notin dans une requete *)
    let rec delete_notin (req : t) : t =
        let rec sup_notin cond =
            match cond with
            | And(c1, c2) -> let cond1, cond2, b1 = sup_notin c1 in
                             let cond3, cond4, b2 = sup_notin c2 in
                             And(cond1, cond3), And(cond2, cond4), b1 || b2
            | Or(c1, c2) -> let cond1, cond2, b1 = sup_notin c1 in
                            let cond3, cond4, b2 = sup_notin c2 in
                            Or(cond1, cond3), Or(cond2, cond4), b1 || b2
            | Rel(_,_,_) -> cond, cond, false
            | In(x,y) -> let y' = delete_notin y in
                         In(x, y') , In(x, y'), false
            | NotIn(x,y) -> let y' = delete_notin y in
                            Rel(x, Eq, x), In(x, y'), true
        in
        match req with
        | Group(req, col) -> Group(delete_notin req, col)
        | Order(req, col) -> Order(delete_notin req, col)
        | Union(r1, r2) -> Union(delete_notin r1, delete_notin r2)
        | Minus(r1, r2) -> Minus(delete_notin r1, delete_notin r2)
        | Where({col = lc; table = ltable ; cond = cond}) ->
                let cond1, cond2, b = sup_notin cond in
                let t =
                    let analyse_col x =
                        match x with
                        | Req(r, name) -> Req(delete_notin r, name)
                        | _ -> x
                    in
                    List.map analyse_col ltable
                in

                match b with
                | true -> if Condition.check_DNF cond then
                              Minus(Where({col = lc; table = t; cond = cond1}),
                                    Where({col = lc; table = t; cond = cond2}))
                          else failwith "Je ne peux rien faire"
                | false -> req
	
	
	
	
	(* retourne une liste de colonne qui sont utilisées dans la requete *)
	let rec col_utilisees (req : t) : string list =
		let rec col_cond c = 
			match c with
			| And(c1, c2) -> (col_cond c1) @ (col_cond c2)
			| Or(c1, c2) -> (col_cond c1) @ (col_cond c2)
			| Rel(ID(a1, b1),_, ID(a2, b2)) -> [a1 ^ "." ^ b1 ; a2 ^ "." ^ b2]
			| In(ID(a, b), req) -> (a ^ "." ^ b) :: (col_utilisees req)
			| NotIn(ID(a, b), req) -> (a ^ "." ^ b) :: (col_utilisees req)
		in
		let rec col_select l =
			match l with
			[] -> []
			| Col(CID(a, b))::q | Col(Max(ID(a, b)))::q | Col(Min(ID(a, b)))::q 
				| Col(Count(ID(a, b)))::q | Col(Avg(ID(a, b)))::q | Col(Sum(ID(a, b)))::q 
					-> (a ^ "." ^ b) :: (col_select q)
            | Rename((CID(a,b) | Max(ID(a,b)) |Min(ID(a,b)) |Count(ID(a,b)) |Avg(ID(a,b)) |Sum(ID(a,b))), _)::q -> (a ^ "." ^ b) :: (col_select q)
        in
		match req with
		| Union(rq1, rq2) -> (col_utilisees rq1) @ (col_utilisees rq2)
		| Minus(rq1, rq2) -> (col_utilisees rq1) @ (col_utilisees rq2)
		| Where(t) -> (col_select t.col) @ (col_cond t.cond) @ (List.fold_left (fun y x -> match x with | Req(r,_) -> (col_utilisees r) @ y | _ -> y) [] t.table)
		| Group(rq, ID(a,b)) -> (a ^ "." ^ b) :: (col_utilisees rq)
		| Order(rq, l) -> (col_utilisees rq) @ (col_select l)
		
		
		
		
		
		
	(* retourne si la requete contient des fonctions d'agrégation
	   return n : nombre de fonction d'agrégation présente
	   0 = absence
	*)
	let nb_agregat_fun (req : t) : int =
		let rec cont_agregat = function
			| [] -> 0
			| Col(Max(_) |Min(_) |Count(_) |Avg(_) | Sum(_))::q -> 1 + (cont_agregat q)
			| Rename((Max(_) |Min(_) |Count(_) |Avg(_) | Sum(_)), _)::q -> 1 + (cont_agregat q)
			| _::q -> cont_agregat q
		in
		match req with
		| Where({col; table ; cond}) -> cont_agregat col
		| _ -> failwith "la fonction contient_agregat doit être utilisée uniquement dans un Select"
			
			
			
	
		

end
