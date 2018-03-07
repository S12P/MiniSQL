

module StringTable =
    struct
        type t = string
        let compare (n1 : t) (n2 : t) = 
            match (n1 = n2) with
            | true -> 1
            | _ -> 0
    end
    
module StringMap = Map.Make(StringTable)
         


type op = Eq | Lt

type idstring = 
      ID of string * string
    | String of string

type cond =
      And of cond * cond
    | Or of cond * cond
    | Rel of idstring * op * idstring
    (*| In of string * string list *)


type requeteWhere =
    {col: idstring list;   (* liste des colonnes que l'on sélectionne *)
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
                print_string l.(i)
            done;
        in
        
        let print_elt l = 
            StringMap.iter (fun key value -> print_string (value ^ "  ")) l;
            print_newline ()
        in
        print_entete table.head;
        List.iter print_elt table.row
    
    
    
    (* Crée une table à partir d'un CSV *)
    let from_csv csv =
        let rec from_list llabels lvalue =
            match llabels, lvalue with
            | [], _ -> StringMap.empty
            | _, [] -> StringMap.empty
            | t1::q1, t2::q2 -> StringMap.add t1 t2 (from_list q1 q2)
        in
        let rec aux lvalues llabels= 
            match lvalues with
            | [] -> []
            | t::q -> (from_list llabels t) :: (aux q llabels)
        in
        match csv with
        | t::q -> {row = aux q t ; head = Array.of_list t}
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
    let union (t1 : t) (t2 : t) : t =
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
      | Rel(s1, Eq, s2) -> (match s1 with
                          | ID (table, colonne) -> table[line].colonne
                          | String(x) -> x
                          ) = (match s2 with
                            | ID (table, colonne) -> table[line].colonne (* valeur dans la base de données *)
                            | String(x) -> x
                          )
      (*| In (s, l) -> if appartient s l then
                          true
                      else false
        *)
    
    
    
    (* Selection de colonnes dans une table selon une table selon une condition 
    On va hacker en fait c'est plus facile ne ne modifiant que le champ head *) 
    let select (t : t) (col : string array) (cond : cond) : t = 
        {col = col ; row = filter (fun x y -> test_cond x cond) t.row}
        
    
end




            
            
            










            