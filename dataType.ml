
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
            | Max of idstring
            | Min of idstring
            | Count of idstring
            | Avg of idstring
            | Sum of idstring

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

and requeteO = requete * column list

and requete =
        | Where of requeteWhere
        | Union of requete * requete
        | Minus of requete * requete
        | Group of requeteO
        | Order of requeteO




