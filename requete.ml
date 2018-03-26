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
                                                let c = match List.hd c with | Col(x) -> x | Rename(x, _) -> x in
                                                ltable, Rel(c, Eq, id), Some cond
                                            else
                                                failwith "Error in the query"
                                    | _ -> failwith "I don't how to normalize this query"
                                end
            | NotIn (_, _) -> failwith "Il ne devrait à cette étape plus y avoir de NOT IN. Si c'est le cas c'est que la requête ne peut pas être traîtée."
        in
        match req with
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
	

end
