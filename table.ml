open DataType
open Cond
open Requete

module Table = struct

    type t = {row: string StringMap.t list; head: string array}


    (* Affiche du mieux possible une table *)
    let print_table table =
        let print_entete l =
            for i = 0 to Array.length l - 1 do
                print_string l.(i);
                print_string "  "
            done;
            print_newline ()
        in

        let print_elt l =
            Array.iter (fun elt-> print_string ((StringMap.find elt l) ^ "  ")) table.head;
            print_newline ()
        in
        print_entete table.head;
        List.iter print_elt (List.rev table.row)




    (* Renome une table, typiquement quand on a FILENALE ID, on renome la table FILENAME en ID *)
    let rename_table (table : t) (nom : string) =
        let a = Array.make (Array.length table.head) "" in
        let coln x =
            String.sub x (String.index x '.') ((String.length x) - (String.index x '.'))
        in
        for i = 0 to Array.length table.head - 1 do
            let colname = coln table.head.(i) in
            a.(i) <- nom ^ colname
        done;
        let rec renamerow l =
            match l with
            | [] -> []
            | t::q -> (StringMap.fold (fun key elt m ->
                                           let colname = coln key in
                                            StringMap.add (nom ^ colname) elt (StringMap.remove key m)) t t) :: (renamerow q)
        in
        {head = a; row = renamerow table.row}


    (* Change le nom d'une colonne lorsque l'on fait un SELECT A.col AS new_name *)
    let rename_col (table : t) (col : string) (new_name : string) : t =
        let tablename = String.sub table.head.(0) 0 (String.index table.head.(0) '.') in
        let a = Array.make (Array.length table.head) "" in
        for i = 0 to Array.length table.head - 1 do
            if table.head.(i) = col then
                a.(i) <- tablename ^ "." ^ new_name
            else
                a.(i) <- table.head.(i)
        done;

        let renameelt key elt m =
            if key = col then
                StringMap.add (tablename ^ "." ^ new_name) elt (StringMap.remove key m)
            else
                m
        in
        let rec renamerow l =
            match l with
            | [] -> []
            | t::q -> (StringMap.fold renameelt t t) :: (renamerow q)
        in
        {head = a; row = renamerow table.row}

	



    (* Crée une table à partir d'un CSV *)
    let from_csv csv table_name lcolused =
    	let rec used col_name lcolused =
    		List.exists (fun x -> x = col_name) lcolused
		in
        let rec from_list llabels lvalue =
            match llabels, lvalue with
            | [], _ -> StringMap.empty
            | _, [] -> StringMap.empty
            | t1::q1, t2::q2 when not (used (table_name ^ "." ^ t1) lcolused)-> StringMap.add (table_name ^ "." ^ t1) t2 (from_list q1 q2)
            | t1::q1, t2::q2 -> from_list q1 q2
        in
        let rec aux lvalues llabels=
            match lvalues with
            | [] -> []
            | t::q -> (from_list llabels t) :: (aux q llabels)
        in
        match csv with
        | t::q -> {row = aux q t ; head = Array.of_list (List.map (fun x -> table_name ^ "." ^ x) t) }
        | _ -> failwith "Error with the CSV file\n"




    (* Un élément appartient-il à une table *)
    let appartient elt (table : string StringMap.t list)=
         List.exists (fun x -> StringMap.equal (fun a b -> a = b) elt x) table

    let appartient_bis (elt : string)  (table : t) x y =
        if Array.length table.head <> 1 then
            failwith "Too much column in the table"
        else
            List.exists (fun a -> (StringMap.find table.head.(0) a) = elt ) table.row




    (* Egalité entre 2 tableux *)
    let array_eq t1 t2 =
        if Array.length t1 <> Array.length t2 then false
        else
          begin
            let b = ref true in
            for i = 0 to Array.length t1 - 1 do
              b := !b && (t1.(i) = t2.(i))
            done;
            !b
          end



    (* Union de deux tables *)
    let union (t1 : t) (t2 : t) : t =
        let rec aux t1 t2 =
            match t2 with
            | [] -> t1
            | t::q when appartient t t1 -> aux t1 q
            | t::q -> aux (t::t1) q
        in
        if array_eq t1.head t2.head then
            { row = aux t1.row t2.row ; head = t1.head }
        else failwith "Union impossible"



    (* Différence de deux tables *)
    let minus (t1 : t) (t2 : t) : t =
      let rec aux t1 t2 res = match t1 with
          | [] -> res
          | t::q when appartient t t2 -> aux q t2 res
          | t::q -> aux q t2 (t::res)
    in
    if array_eq t1.head t2.head then
        { row = aux t1.row t2.row []; head = t1.head}
    else failwith "Minus impossible"




	(* Calcule le résultat d'une fonction d'agrégation dans le cas où il n'y en a qu'une seule *)
	let compute_agreg fct row =
		match fct with
		| Min(ID(a,b)) -> begin
			try
				[StringMap.(empty 
							|> add (a ^ ".Min(" ^ b ^ ")")
            						(string_of_int (List.fold_left (fun x y -> min x (int_of_string(StringMap.find (a ^ "." ^ b) y))) 
            														(int_of_string (StringMap.find (a ^ "." ^ b) 
            														(List.hd row))) 
													row)))]
			with 
			| _ -> [StringMap.(empty |> add (a ^ ".Min(" ^ b ^ ")")
                        (List.fold_left (fun x y -> min x (StringMap.find (a ^ "." ^ b) y)) (StringMap.find (a ^ "." ^ b) (List.hd row)) row))]
        	end
    	| Max(ID(a,b)) -> begin
			try
				[StringMap.(empty 
							|> add (a ^ ".Max(" ^ b ^ ")")
            						(string_of_int (List.fold_left (fun x y -> max x (int_of_string(StringMap.find (a ^ "." ^ b) y))) 
            														(int_of_string (StringMap.find (a ^ "." ^ b) 
            														(List.hd row))) 
													row)))]
			with 
			| _ -> [StringMap.(empty |> add (a ^ ".Max(" ^ b ^ ")")
                        (List.fold_left (fun x y -> max x (StringMap.find (a ^ "." ^ b) y)) (StringMap.find (a ^ "." ^ b) (List.hd row)) row))]
        	end
				
		| Count(ID(a,b)) -> [StringMap.(empty |> add (a ^ ".Count(" ^ b ^ ")") (string_of_int (List.length row)))]
		| Sum(ID(a, b)) -> begin
			try 
				[StringMap.(empty 
							|> add (a ^ ".Sum(" ^ b ^ ")")
            						(string_of_int (List.fold_left (fun x y -> x + (int_of_string(StringMap.find (a ^ "." ^ b) y))) 
            														0 row)))]
            with 
            	_ -> failwith "Impossible d'utiliser SUM sur une colonne qui ne contient pas que des entiers"
            end
    	| Avg(ID(a, b)) -> begin
      		try 
      			[StringMap.(empty 
      						|> add (a ^ ".Sum(" ^ b ^ ")")
              						(string_of_float ((List.fold_left (fun x y -> x +. (float_of_string(StringMap.find (a ^ "." ^ b) y))) 0. row)
              											 /. (float_of_int (List.length row)))))]
        	with 
        		_ -> failwith "Impossible d'utiliser AVG sur une colonne qui ne continet pas que des entiers"
      		end
		| _ -> failwith "Cas impossible normalement"





	let rec select_agregate l_col table = 
		let f x y z = Some y in
		match l_col with
		| [] -> StringMap.empty
		| Col(CID(x, y)) :: q -> StringMap.add (x ^ "." ^ y) (StringMap.find (x ^ "." ^ y) (List.hd table)) (select_agregate q table)
        | Col(t)::q -> StringMap.union f (List.hd (compute_agreg t table)) (select_agregate q table)
        | Rename(CID(x, y), new_name) :: q -> StringMap.add (x ^ "." ^ new_name) (StringMap.find (x ^ "." ^ y) (List.hd table)) (select_agregate q table)
        | Rename(t, _)::q -> StringMap.union f (List.hd (compute_agreg t table)) (select_agregate q table)





    (* produit cartésien de 2 tables *)
    let rec reduce_table (table1 : t) (table2 : t) =
        let rec union elt1 elt2 =
            StringMap.fold (fun x y m -> StringMap.add  x y m) elt2 elt1
        in
        let rec add elt t l =
            match t with
            | [] -> l
            | t::q -> (add elt q ((union elt t)::l))
        in
        let rec unify t1 t2 l =
            match t1 with
            | [] ->  l
            | t::q -> (unify q t2 ((add t t2 []) @ l))
        in
        let h = Array.append table1.head table2.head in
        let r = unify table1.row table2.row [] in
        {row = r; head = h}




        (* produit cartésien d'une liste de tables *)
    and reduce_table_list ltable = match ltable with
        | [] -> failwith "erreur"
        | [t] -> t
        | t1::t2::q -> reduce_table_list ((reduce_table t1 t2)::q)
        
        
        
        
    and compute ast : t =
        match ast with
        | Where({col = x; table = y; cond = z}) -> select x y z
        | Union(ast1, ast2) -> union (compute ast1) (compute ast2)
        | Minus(ast1, ast2) -> let a = compute ast1 in let b = compute ast2 in minus a b
        | Order(ast, col) -> order (compute ast) col
        | Group(Where({col = x; table = y; cond = z}), col) -> select ~groupby:col x y z
        | Group(ast, col) -> failwith "erreur avec le groupby"
        






	
	(* Selection de colonnes dans une table selon une table selon une condition *)
    and select ?groupby  (col : column list) (tab : liretable list) (cond : cond)  : t =
        let lcolused = Requete.col_utilisees (Where({ col = col ; table = tab ; cond = cond})) in
        let lire_table t = match t with
            | File(f, new_name) ->
                begin

                    let file = open_in f in
                    let file_name = String.sub f 0 (String.index f '.') in
                    let tab = rename_table (from_csv (Csv.load_in file) file_name lcolused) new_name in
                    Pervasives.close_in file;
                    tab
                end
            | Req(table, new_name) -> rename_table (compute table) new_name
        in

        let liste_table = List.map lire_table tab in
        let table = reduce_table_list liste_table in
        let head = Array.of_list (List.map (fun x -> match x with 
                      								Col(CID(a, b)) -> a ^ "." ^ b
                      								| Col Max(ID(a, b)) -> a ^ ".Max(" ^ b ^ ")"
                      								| Col Min(ID(a, b)) -> a ^ ".Min(" ^ b ^ ")"
                      								| Col Count(ID(a, b)) -> a ^ ".Count(" ^ b ^ ")"
                      								| Col Avg(ID(a, b)) -> a ^ ".Avg(" ^ b ^ ")"
                      								| Col Sum(ID(a, b)) -> a ^ ".Sum(" ^ b ^ ")"
                      								| Rename((CID(a,b) | Max(ID(a, b)) | Min(ID(a, b)) | Count(ID(a, b)) | Avg(ID(a, b)) | Sum(ID(a, b))), _) -> a ^ "." ^ b
                                           ) col) in

		(* colonnes qui correspondent à la condition du where *)
		let row = List.filter (fun x -> Condition.test_cond x cond) table.row in
        let nbagregatfun = Requete.nb_agregat_fun (Where({col = col; table = [] ; cond = cond})) in
        
        
        (* Pas de fonction d'agrégation ici *)
        if nbagregatfun = 0 then
        	let newtable = {head = head ; row = row} in
			List.fold_right (fun a b -> match a with
				                        | Rename(CID(t,c), new_name) -> rename_col b (t ^ "." ^ c) new_name
				                        | _ -> b)
				             col newtable
				             
				             
		(* premier cas, on n'a qu'une seule fonction d'agrégat dans la requête, il n'y a pas de group by *)		             
        else 
        	begin
       		if (List.length col) = nbagregatfun && nbagregatfun = 1 then
				let row = match List.hd col with
					| Col(x) -> compute_agreg x row
					| Rename(x, _) -> compute_agreg x row 
				in
				let newtable = {head = head ; row = row} in
				List.fold_right (fun a b -> match a with
				                            | Rename(CID(t,c), new_name) -> rename_col b (t ^ "." ^ c) new_name
				                            | Rename(Min(ID(t,c)), new_name) -> rename_col b (t ^ ".Min(" ^ c ^ ")") new_name
				                            | Rename(Max(ID(t,c)), new_name) -> rename_col b (t ^ ".Max(" ^ c ^ ")") new_name
				                            | Rename(Avg(ID(t,c)), new_name) -> rename_col b (t ^ ".Avg(" ^ c ^ ")") new_name
				                            | Rename(Count(ID(t,c)), new_name) -> rename_col b (t ^ ".Count(" ^ c ^ ")") new_name
				                            | Rename(Sum(ID(t,c)), new_name) -> rename_col b (t ^ ".Sum(" ^ c ^ ")") new_name
				                            | _ -> b)
				                  col newtable
          
          
          (* TODO: cas où on a un group by *)
          else
          	match groupby with
          	| None -> failwith "groupby doit être défini dans la fonction Select!"
          	| Some (ID(x, y)) -> (* x est la colonne sur laquelle on regroupe *)
          		begin 
          			let namecol = x ^ "." ^ y in
                    let rec app l x = match l with | [] -> false | t::_ when t = x -> true | _::q -> app q x in
          			let rec listlignediff l lf = match l with
          			| [] -> lf
          			| t::q when app lf (StringMap.find namecol t) -> listlignediff q lf
          			| t::q -> listlignediff q ((StringMap.find namecol t)::lf) 
          			in
          			let lignes = listlignediff row [] in
          			let row = List.fold_left (fun l x -> (select_agregate col (List.filter (fun y -> (StringMap.find namecol y) = x ) row) :: l )) [] lignes in
          			{row; head}
          		end
          	
          	
          	
          	(*let newtable = {head ; row} in
			List.fold_right (fun a b -> match a with
				                        | Rename(CID(t,c), new_name) -> rename_col b (t ^ "." ^ c) new_name
				                        | _ -> b)
				             col newtable*)
		end





	(* effectue le order by *)
	and order (req : t) (col : column list) : t =
		let colr = List.rev col in
		let rec comp col x y =
			match col with
			| [] -> 0
			| Col(CID(a, b)) :: q ->
				(if StringMap.find (a ^ "." ^ b) x < StringMap.find (a ^ "." ^ b) y then 1
				else if StringMap.find (a ^ "." ^ b) x = StringMap.find (a ^ "." ^ b) y then comp q x y
				else -1)
			| _ -> failwith ""
	in
		match colr with
		| [] -> req
		| Col(CID(a, b))::q ->  order ({head = req.head; row = List.sort (comp colr) req.row}) q
		| _ -> failwith ""





	(* effectue un group by *)
    and group (req : t) (col : idstring) = 
    	req


end

