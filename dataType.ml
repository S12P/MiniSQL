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
        | _ -> failwith "Erreur with the CSV"
end
            
            
            
            