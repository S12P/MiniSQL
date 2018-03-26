open DataType
open Cond

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
        List.iter print_elt table.row




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

	
	(* Supprime une colonne de la liste *)
	let supprime_col (table : t) (col_name : string) : t =
		let sup ligne =
			StringMap.remove (col_name) ligne
		in
		{head = table.head ; row = List.map sup table.row}
		
	let rec supprime_cols (table : t) (cols_name : string list) : t =
		match cols_name with
		  [] -> table
		| t :: q -> supprime_cols (supprime_col table t) q
		
		
	let rec col_utilisees req : string list =
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
			| Col(ID(a, b))::q | Max(ID(a, b))::q | Min(ID(a, b))::q | Count(ID(a, b))::q | Avg(ID(a, b))::q | Sum(ID(a, b))::q -> (a ^ "." ^ b) :: (col_select q)
            | Rename(ID(a,b), _)::q -> (a ^ "." ^ b) :: (col_select q)
        in
		match req with
		| Union(rq1, rq2) -> (col_utilisees rq1) @ (col_utilisees rq2)
		| Minus(rq1, rq2) -> (col_utilisees rq1) @ (col_utilisees rq2)
		| Where(t) -> (col_select t.col) @ (col_cond t.cond)
		| Group(rq, l) | Order(rq, l) -> (col_utilisees rq) @ (col_select l)
		


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
        | Group(ast, col) -> group (compute ast) col






	
	(* Selection de colonnes dans une table selon une table selon une condition *)
    and select (col : column list) (tab : liretable list) (cond : cond) : t =
        let lcolused = col_utilisees (Where({ col = col ; table = tab ; cond = cond})) in
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
        let head = Array.of_list (List.map (fun x -> match x with | Col(ID(a, b)) -> a ^ "." ^ b
                                                                  | Rename(ID(a,b), new_name) -> a ^ "." ^ b
                                                                  | Max(ID(a, b)) -> a ^ "." ^ b
                                                                  | Min(ID(a, b)) -> a ^ "." ^ b
                                                                  | Count(ID(a, b)) -> a ^ "." ^ b
                                                                  | Avg(ID(a, b)) -> a ^ "." ^ b
                                                                  | Sum(ID(a, b)) -> a ^ "." ^ b
                                           ) col) in

        (*let row = List.filter (fun x -> test_cond x cond) table.row in
        let newtable = {head = head ; row = row} in
        (* min max etc *)*)

	let row = List.filter (fun x -> Condition.test_cond x cond) table.row in

		let row2 c = match c with
			| [Min(ID(a,b))] -> begin
						try [StringMap.(empty |> add (a ^ "." ^ b)
		                (string_of_int (List.fold_left (fun x y -> min x (int_of_string(StringMap.find (a ^ "." ^ b) y))) (int_of_string (StringMap.find (a ^ "." ^ b) (List.hd row))) row)))]
		                with _ -> [StringMap.(empty |> add (a ^ "." ^ b)
		                            (List.fold_left (fun x y -> min x (StringMap.find (a ^ "." ^ b) y)) (StringMap.find (a ^ "." ^ b) (List.hd row)) row))]
		                end
			| [Max(ID(a,b))] -> begin
						try [StringMap.(empty |> add (a ^ "." ^ b)
		                (string_of_int (List.fold_left (fun x y -> max x (int_of_string (StringMap.find (a ^ "." ^ b) y))) (int_of_string (StringMap.find (a ^ "." ^ b) (List.hd row))) row)))]
		                with _ -> [StringMap.(empty |> add (a ^ "." ^ b)
		                            (List.fold_left (fun x y -> max x (StringMap.find (a ^ "." ^ b) y)) (StringMap.find (a ^ "." ^ b) (List.hd row)) row))]
		                    end
			| [Count(ID(a,b))] -> [StringMap.(empty |> add (a ^ "." ^ b) (string_of_int (List.length row)))]
		  | [Sum(ID(a, b))] -> begin
						try [StringMap.(empty |> add (a ^ "." ^ b)
		                (string_of_int (List.fold_left (fun x y -> x + (int_of_string(StringMap.find (a ^ "." ^ b) y))) 0 row)))]
		                with _ -> failwith "SUM ne marche pas car ce n'est pas des nombres"
		                end
		  | [Avg(ID(a, b))] -> begin
		          	   try [StringMap.(empty |> add (a ^ "." ^ b)
		                  (string_of_float ((List.fold_left (fun x y -> x +. (float_of_string(StringMap.find (a ^ "." ^ b) y))) 0. row) /. (float_of_int (List.length row)))))]
                      with _ -> failwith "AVG ne marche pas car ce n'est pas des nombres"
                  end
          | _ -> row 
         in
        (* col ? column list ? *)
        let newtable = {head = head ; row = row2 col} in
        List.fold_right (fun a b -> match a with
                                    | Rename(ID(t,c), new_name) -> rename_col b (t ^ "." ^ c) new_name
                                    | _ -> b)
                          col newtable









     and order (req : t) (col : column list) : t =
          let colr = List.rev col in
          let rec comp col x y =
          	match col with
          	| [] -> 0
          	| Col(ID(a, b)) :: q ->
		        (if StringMap.find (a ^ "." ^ b) x < StringMap.find (a ^ "." ^ b) y then 1
		        else if StringMap.find (a ^ "." ^ b) x = StringMap.find (a ^ "." ^ b) y then comp q x y
		        else -1)
	        | _ -> failwith ""
        in
            match colr with
              | [] -> req
              | Col(ID(a, b))::q ->  order ({head = req.head; row = List.sort (comp colr) req.row}) q
              | _ -> failwith ""



    and group (req : t) (col : column list) :t = order req col


end
