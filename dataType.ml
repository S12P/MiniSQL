

module StringTable =
    struct
        type t = string
        let compare = compare
    end

module StringMap = Map.Make(StringTable)



type op = Eq | Lt

and idstring = ID of string * string

and column = Col of idstring
            | Rename of idstring * string

and cond =
      And of cond * cond
    | Or of cond * cond
    | Rel of idstring * op * idstring
    | In of idstring * requete
    | NotIn of idstring * requete

and liretable = | File of string * string
                | Req of requete * string


and requeteWhere =
    {col: column list;   (* liste des colonnes que l'on sélectionne *)
     table: liretable list;     (* table dans le join *)
     cond: cond;         (* condition dans le where *)
    }

and requete =
        | Where of requeteWhere
        | Union of requete * requete
        | Minus of requete * requete






(* Représente une table SQL *)
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





    (* Crée une table à partir d'un CSV *)
    let from_csv csv table_name =
        let rec from_list llabels lvalue =
            match llabels, lvalue with
            | [], _ -> StringMap.empty
            | _, [] -> StringMap.empty
            | t1::q1, t2::q2 -> StringMap.add (table_name ^ "." ^ t1) t2 (from_list q1 q2)
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






    and test_cond line cond : bool = (*permet de tester la condition du where*)
        match cond with
          | And (c1, c2) -> (test_cond line c1 ) && (test_cond line c2 )
          | Or(c1, c2) -> (test_cond line c1 ) || (test_cond line c2 )
          | Rel(s1, Eq, s2) -> let ID(x1, y1) = s1 and ID(x2, y2) = s2 in
                            (StringMap.find (x1 ^ "." ^ y1) line) = (StringMap.find (x2 ^ "." ^ y2) line)
          | Rel(s1, Lt, s2) -> let ID(x1, y1) = s1 and ID(x2, y2) = s2 in
                                (StringMap.find (x1 ^ "." ^ y1) line) < (StringMap.find (x2 ^ "." ^ y2) line)
          | _ -> failwith "Impossible normalement"



    and compute ast : t =
        match ast with
        | Where({col = x; table = y; cond = z}) -> select x y z
        | Union(ast1, ast2) -> union (compute ast1) (compute ast2)
        | Minus(ast1, ast2) -> let a = compute ast1 in let b = compute ast2 in minus a b








    (* Selection de colonnes dans une table selon une table selon une condition *)
    and select (col : column list) (tab : liretable list) (cond : cond) : t =
        let lire_table t = match t with
            | File(f, new_name) ->
                begin

                    let file = open_in f in
                    let file_name = String.sub f 0 (String.index f '.') in
                    let tab = rename_table (from_csv (Csv.load_in file) file_name) new_name in
                    Pervasives.close_in file;
                    tab
                end
            | Req(table, new_name) -> rename_table (compute table) new_name
        in

        let liste_table = List.map lire_table tab in
        let table = reduce_table_list liste_table in
        let head = Array.of_list (List.map (fun x -> match x with | Col(ID(a, b)) -> a ^ "." ^ b
                                                                  | Rename(ID(a,b), new_name) -> a ^ "." ^ b
                                           ) col) in
        let row = List.filter (fun x -> test_cond x cond) table.row in
        let newtable = {head = head ; row = row} in
        List.fold_right (fun a b -> match a with
                                    | Col(ID(_, _)) -> b
                                    | Rename(ID(t,c), new_name) -> rename_col b (t ^ "." ^ c) new_name)
                          col newtable









    (* Normalise pour enlever les In *)
    let rec normalize_req (req : requete) : requete =
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






    (* Vérifie sur une condition est en forme DNF *)
    let rec check_DNF cond =
        let rec clause = function
            | Or(c1, c2) -> (clause c1) && (clause c2)
            | And(c1, c2) -> false
            | Rel(_,_,_) -> true
            | In(_, _) -> true
            | NotIn(_,_) -> true
        in
        match cond with
        | And(c1, c2) -> (check_DNF c1) && (check_DNF c1)
        | _ -> clause cond




    (* Supprime les notin dans une requete *)
    let rec delete_notin req =
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
                | true -> if check_DNF cond then
                              Minus(Where({col = lc; table = t; cond = cond1}),
                                    Where({col = lc; table = t; cond = cond2}))
                          else failwith "Je ne peux rien faire"
                | false -> req


end
