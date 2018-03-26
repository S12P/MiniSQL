open Csv
open DataType
open Table
open Requete


let lexbuf c = Lexing.from_string c

let parse c = Parser.main Lexer.token (lexbuf c)



let _ =



    while true do
        Printf.printf "> ";
        flush_all ();
        let x = input_line stdin in
        (* On supprime les notin de la requete *)
        let requeteparsee = Requete.delete_notin (parse x) in
        (* On supprime les in de la requete *)
        let requeteparsee = Requete.normalize_req requeteparsee in
        
        (* On affiche le résultat de la requête *)
        Table.print_table (Table.compute requeteparsee)
    done;
