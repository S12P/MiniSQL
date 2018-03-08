

module StringTable =
    struct
        type t = string
        let compare = compare
    end

module StringMap = Map.Make(StringTable)



type op = Eq (*| Lt*)

type idstring = ID of string * string

type column = Col of idstring
            | Rename of idstring * string

type cond =
      And of cond * cond
    | Or of cond * cond
    | Rel of idstring * op * idstring
    (*| In of string * string list *)


type requeteWhere =
    {col: column list;   (* liste des colonnes que l'on sélectionne *)
     table: string list;     (* table dans le join *)
     cond: cond;         (* condition dans le where *)
    }

type requete =
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
    let appartient elt table =
         List.exists (fun x -> StringMap.equal (fun a b -> a = b) elt x) table


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
        let rec aux t1 t2 =
            match t2 with
            | [] -> t1
            | t::q when appartient t t1 -> aux (t::t1) q
            | t::q -> aux t1 q
        in
        if array_eq t1.head t2.head then
            { row = aux t1.row t2.row ; head = t1.head }
        else failwith "Minus impossible"




    let rec test_cond line cond : bool = (*permet de tester la condition du where*)
        match cond with
          | And (c1, c2) -> (test_cond line c1 ) && (test_cond line c2 )
          | Or(c1, c2) -> (test_cond line c1 ) || (test_cond line c2 )
          | Rel(s1, Eq, s2) -> let ID(x1, y1) = s1 and ID(x2, y2) = s2 in
                                (StringMap.find (x1 ^ "." ^ y1) line) = (StringMap.find (x2 ^ "." ^ y2) line)
          (*| In (s, l) -> if appartient s l then
                              true
                          else false
            *)


        (* produit cartésien de 2 tables *)
    let rec reduce_table table1 table2 =
        let rec union elt1 elt2 =
            StringMap.fold (fun x y m -> StringMap.add  x y m) elt2 elt1
        in
        let rec add elt t =
            match t with
            | [] -> []
            | t::q -> (union elt t)::(add elt q)
        in
        let rec unify t1 t2 =
            match t1 with
            | [] ->  []
            | t::q -> (add t t2)@(unify q t2)
        in
        {row = unify table1.row table2.row; head = Array.append table1.head table2.head}

    (* produit cartésien d'une liste de tables *)
    let rec reduce_table_list ltable = match ltable with
        | [] -> failwith "erreur"
        | [t] -> t
        | t1::t2::q -> reduce_table_list ((reduce_table t1 t1)::q)




    (* Renome une table, typiquement quand on a FILENALE ID, on renome la table FILENAME en ID *)
    let rename_table (table : t) (nom : string) =
        let a = Array.make (Array.length table.head) "" in
        for i = 0 to Array.length table.head - 1 do
            let colname = String.sub table.head.(i) (String.index table.head.(i) '.' + 1) (String.length table.head.(i) - 1) in
            a.(i) <- colname ^ "." ^ colname
        done;
        let rec renamerow l =
            match l with
            | [] -> []
            | t::q -> (StringMap.fold (fun key elt m ->
                                           let colname = String.sub key (String.index key '.' + 1) (String.length key - 1) in
                                            StringMap.add (nom ^ "." ^ colname) elt (StringMap.remove key m)) t t) :: (renamerow q)
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








    (* Selection de colonnes dans une table selon une table selon une condition
    On va hacker en fait c'est plus facile ne ne modifiant que le champ head *)
    let select (col : column list) (tab : string list) (cond : cond) ltable : t =
        let liste_table = List.map (fun x ->  StringMap.find x ltable) tab in
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








end






let rec compute ast ltable =
    match ast with
    | Where({col = x; table = y; cond = z}) -> Table.select x y z ltable
    | Union(ast1, ast2) -> Table.union (compute ast1 ltable) (compute ast2 ltable)
    | Minus(ast1, ast2) -> Table.minus (compute ast1 ltable) (compute ast2 ltable)
