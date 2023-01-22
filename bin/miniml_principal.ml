(* ouverture de la "library" definie dans lib/dune *)
open Miniml

(* ouverture de modules de la library Miniml *)
open Miniml_lexer
open Miniml_parser
open Miniml_typer

(* ******** à compléter ********* *)

let run_miniml tokens = match Solution.uncons (parse_miniml tokens) with
  | None -> (Format.printf "** parsing failed ! **@.")
  | Some (p, q) -> 
    begin
      print_solutions (Solution.cons p q);
    end

let main () = 
    if (Array.length Sys.argv > 1)
      then 
        let flux_of_tokens = read_miniml_tokens_from_file (Sys.argv.(1)) 
        in run_miniml flux_of_tokens;
      else
        let rec loop () = 
          Format.printf "programme?@.";
          flush stdout;
          let l = read_line () in
          let flux_of_tokens = read_miniml_tokens_from_string l
          in run_miniml flux_of_tokens;
          loop ()
        in loop ();;

main ();;