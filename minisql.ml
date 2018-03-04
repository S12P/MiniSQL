open Csv
open DataType


let read_data () =
    let argc = Array.length Sys.argv in
    let tables = StringMap.empty in  (* dictionnaire de toutes les tables *)
    for i = 1 to argc - 1 do
        let file = open_in Sys.argv.(i) in
        let file_name = String.sub Sys.argv.(i) 0 (String.index Sys.argv.(i) '.') in
        let csvfile = Table.from_csv (Csv.load_in file) in
        let tables = StringMap.add file_name csvfile tables in
        print_string ("File " ^ file_name ^ ".csv loaded");
        
    done;



let _ =
    read_date ();
    
    
    