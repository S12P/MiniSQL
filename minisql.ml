open Csv
open DataType


let lexbuf c = Lexing.from_string c

let parse c = Parser.main Lexer.token (lexbuf c)

let scan_string () = Scanf.scanf " %s" (fun x -> x)



let _ =
    let argc = Array.length Sys.argv in
    let tables = ref StringMap.empty in  (* dictionnaire de toutes les tables *)
    for i = 1 to argc - 1 do
        let file = open_in Sys.argv.(i) in
        let file_name = String.sub Sys.argv.(i) 0 (String.index Sys.argv.(i) '.') in
        let csvfile = Table.from_csv (Csv.load_in file) file_name in
        tables := StringMap.add file_name csvfile !tables;
        print_string ("File " ^ file_name ^ ".csv loaded.\n");
        Pervasives.close_in file
    done;



    while true do
        Printf.printf "> ";
        flush_all ();
        let x = input_line stdin in
        let requeteparsee = parse x in
        Table.print_table (compute requeteparsee (!tables))
    done;

(* tables : dictionnaires de toutes les tables *)
