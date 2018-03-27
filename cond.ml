open DataType

module Condition = struct


	(* Type d'une condition définie dans DataType *)
	type t = cond
	
	

	(* Vérifie si une condition est en CNF *)	
	let rec check_DNF (cond : t) : bool =
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
        
        
    
    
    
    (* teste si une condition cond est vérifiée sur une ligne *)    
    let rec test_cond line (cond : t) : bool = 
        match cond with
          | And (c1, c2) -> (test_cond line c1 ) && (test_cond line c2 )
          | Or(c1, c2) -> (test_cond line c1 ) || (test_cond line c2 )
          | Rel(s1, Eq, s2) -> let ID(x1, y1) = s1 and ID(x2, y2) = s2 in
                            (StringMap.find (x1 ^ "." ^ y1) line) = (StringMap.find (x2 ^ "." ^ y2) line)
          | Rel(s1, Lt, s2) -> begin
          						   let ID(x1, y1) = s1 and ID(x2, y2) = s2 in
                               	   try 
                               	   	   int_of_string (StringMap.find (x1 ^ "." ^ y1) line) < int_of_string (StringMap.find (x2 ^ "." ^ y2) line)
                                   with 
                                       _ -> (StringMap.find (x1 ^ "." ^ y1) line) < (StringMap.find (x2 ^ "." ^ y2) line)
        					   end
		 | _ -> failwith "Normalement ce cas est impossible. Il n'y a plus de In ou de Not In à cette étape"
		 
		 
		 
		 
	 let rec cond_to_list (cond : t) =
	 	match cond with
	 	| Rel(c1, (Eq | Lt), c2) -> [cond]
	 	| And(c1, c2) -> (cond_to_list c1) @ (cond_to_list c2)
	 	| _ -> failwith "Pour l'instant ce cas n'est pas prévu"
		 
	
	
    
end