open Csv
open DataType


let lexbuf c = Lexing.from_string c

let parse c = Parser.main Lexer.token (lexbuf c)

let scan_string () = Scanf.scanf " %s" (fun x -> x)



let _ =



    while true do
        Printf.printf "> ";
        flush_all ();
        let x = input_line stdin in
        let requeteparsee = Table.normalize_req (parse x) in
        Table.print_table (Table.compute requeteparsee)
    done;

(* tables : dictionnaires de toutes les tables *)
