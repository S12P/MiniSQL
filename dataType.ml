
module StringTable =
    struct
        type t = string
        let compare = compare
    end

module StringMap = Map.Make(StringTable)



type op = Eq | Lt

type idstring = ID of string * string

type coltype = | CID of string * string
			   | Max of idstring
			   | Min of idstring
		       | Count of idstring
		       | Avg of idstring
		       | Sum of idstring

type column = Col of coltype
            | Rename of coltype * string
            

type cond =
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
        | Group of requete * idstring        (* group by que sur une seule colonne *)
        | Order of requete * column list




