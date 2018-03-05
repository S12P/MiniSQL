

module StringTable =
    struct
        type t = string
        let compare (n1 : t) (n2 : t) = 
            match (n1 = n2) with
            | true -> 1
            | _ -> 0
    end
    
module StringMap = Map.Make(StringTable)
         
        
module Table = struct

    type t = string StringMap.t list
    
    let print_table table =
        let print_entete l =
            StringMap.iter (fun key value -> 
                            print_string (key ^ "  ")) l;
            print_newline ()
        in
        
        let print_elt l = 
            StringMap.iter (fun key value -> print_string (value ^ "  ")) l;
            print_newline ()
        in
        print_entete (List.hd table);
        List.iter print_elt table
    
    
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
        | t::q -> aux q t
        | _ -> failwith "Error with the CSV file\n"
        
    
    let appartient elt table =
         List.exists (fun x -> StringMap.equal (fun a b -> a = b) elt x) table
     
     
    (* Union de deux tables *)    
    let rec union t1 t2 =
        match t2 with
        | [] -> t1
        | t::q when appartient t t1 -> union t1 q
        | t::q -> union (t::t1) q
        
    
end




            
            
            
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











            