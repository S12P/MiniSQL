open Csv
open DataType
open Table
open Requete


let lexbuf c = Lexing.from_string c

let parse c = Parser.main Lexer.token (lexbuf c)

let scan_string () = Scanf.scanf " %s" (fun x -> x)



let _ =



    while true do
        Printf.printf "> ";
        flush_all ();
        let x = input_line stdin in
        let requeteparsee = Requete.delete_notin (parse x) in
          
        let requeteparsee = Requete.normalize_req requeteparsee in
        Table.print_table (Table.compute requeteparsee)
    done;
