open Miniml_types
open Miniml_lexer
open Lazyflux

(* les deux types de flux utiisés: le flux à parser et le flux des solutions *)
module Flux = Flux;;
module Solution = Flux;;

(* types des parsers généraux *)
type ('a, 'b) result = ('b * 'a Flux.t) Solution.t;;
type ('a, 'b) parser = 'a Flux.t -> ('a, 'b) result;;

(* interface des parsers: combinateurs de parsers et parsers simples *)
module type Parsing =
  sig
    val map : ('b -> 'c) -> ('a, 'b) parser -> ('a, 'c) parser

    val return : 'b -> ('a, 'b) parser

    val ( >>= ) : ('a, 'b) parser -> ('b -> ('a, 'c) parser) -> ('a, 'c) parser

    val zero : ('a, 'b) parser

    val ( ++ ) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser

    val run : ('a, 'b) parser -> 'a Flux.t -> 'b Solution.t

    val pvide : ('a, unit) parser

    val ptest : ('a -> bool) -> ('a, 'a) parser

    val ( *> ) : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'b * 'c) parser
  end

(* implantation des parsers, comme vu en TD. On utilise les opérations *)
(* du module Flux et du module Solution                                *)
module Parser : Parsing =
  struct
    let map fmap parse f = Solution.map (fun (b, f') -> (fmap b, f')) (parse f);; 

    let return b f = Solution.return (b, f);;

    let (>>=) parse dep_parse = fun f -> Solution.(parse f >>= fun (b, f') -> dep_parse b f');;

    let zero f = Solution.zero;;

    let (++) parse1 parse2 = fun f -> Solution.(parse1 f ++ parse2 f);;

    let run parse f = Solution.(map fst (filter (fun (b, f') -> Flux.uncons f' = None) (parse f)));;

    let pvide f =
      match Flux.uncons f with
      | None   -> Solution.return ((), f)
      | Some _ -> Solution.zero;;

    let ptest p f =
      match Flux.uncons f with
      | None        -> Solution.zero
      | Some (t, q) -> if p t then Solution.return (t, q) else Solution.zero;;

    let ( *> ) parse1 parse2 = fun f ->
      Solution.(parse1 f >>= fun (b, f') -> parse2 f' >>= fun (c, f'') -> return ((b, c), f''));;
  end

  

(* Fonction de lecture d'un fichier.    *)
(* Produit le flux des lexèmes reconnus *)
let read_miniml_tokens_from_file filename : token Flux.t =
  try
    (* open the file *)
    let chan = open_in filename in
    let buf = Lexing.from_channel chan in
    line_g := 1;
    let next_token () =
      try
        let next = token buf in
        if next = EOF
        then
          begin
            close_in chan;
            None
          end
        else
          Some (next, ())
   with
   | ErreurLex msg ->
      begin
        close_in chan;
        raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
      end in
    Flux.unfold next_token ()
 with
    | Sys_error _ -> raise (ErreurLecture (Format.sprintf "ERREUR : Impossible d'ouvrir le fichier '%s' !" filename))
;;

(* Fonction de lecture d'un buffer.   *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_lexbuf buf : token Flux.t =
  line_g := 1;
  let next_token () =
    try
      let next = token buf in
      if next = EOF
      then
        begin
          None
        end
      else
        Some (next, ())
    with
    | ErreurLex msg ->
       begin
         raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
       end in
  Flux.unfold next_token ()
;;

(* Fonction de lecture d'une chaîne.  *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_string chaine : token Flux.t =
  read_miniml_tokens_from_lexbuf (Lexing.from_string chaine)
;;

(* Fonctions auxiliaires de traitement des lexèmes *)
(* contenant une information: IDENT, BOOL et INT   *)
let isident =
  function IDENT _     -> true
         | _           -> false
let isbool =
  function BOOL _      -> true
         | _           -> false
let isint =
  function INT _       -> true
         | _           -> false

let unident =
  function
  | IDENT id    -> id
  | _           -> assert false
let unbool =
  function
  | BOOL b      -> b
  | _           -> assert false   
let unint =
  function
  | INT i       -> i
  | _           -> assert false




open Parser

(* 'droppe' le resultat d'un parser et le remplace par () *)
let drop p = map (fun x -> ()) p

let p_token token = drop (ptest ((=) token))

(* Definition des parsers des tokens *)
let p_let = p_token LET
let p_rec = p_token REC
let p_in = p_token IN

let p_fun = p_token FUN
let p_to = p_token TO

let p_if = p_token IF
let p_then = p_token THEN
let p_else = p_token ELSE
let p_and = p_token AND
let p_or = p_token OR
let p_eq = p_token EQU
let p_not_eq = p_token NOTEQ
let p_sup_or_eq = p_token SUPEQ
let p_sup = p_token SUP
let p_inf_or_eq = p_token INFEQ
let p_inf = p_token INF

let p_plus = p_token PLUS
let p_moins = p_token MOINS
let p_mult = p_token MULT
let p_div = p_token DIV

let p_concat = p_token CONCAT
let p_cons = p_token CONS


let p_cro_ouv = p_token CROO
let p_cro_fer = p_token CROF
let p_par_fer = p_token PARF
let p_par_ouv = p_token PARO


let p_eof = pvide;;

let p_ident = (ptest isident) >>= fun token -> return (unident token);;

let p_bool = (ptest isbool) >>= fun token -> return (unbool token);;

let p_int = (ptest isint) >>= fun token -> return (unint token);;


let rec parse_Expr = fun flux ->
  (
    (
      p_let *> p_ident *> p_eq *> parse_Expr *> p_in *> parse_Expr 
      >>= fun (((((_, id), _), e1), _), e2) -> return (ELet (id, e1, e2))
    ) 
    ++
    (
      p_let *> p_rec *> p_ident *> p_eq *> parse_Expr *> p_in *> parse_Expr
      >>= fun ((((((_, _), id), _), e1), _), e2) -> return (ELetrec (id, e1, e2))
    )
    ++
    (
      parse_constant 
      >>= fun c -> return (EConstant(c))
    )
    
  ) flux
and parse_constant = fun token_flux -> 
  (
    (
      p_int >>= fun i -> return (CEntier(i))
    ) 
    ++
    (
      p_bool >>= fun b -> return (CBooleen(b))
    ) 
    ++
    (
      p_cro_ouv *> p_cro_fer >>= fun (_, _) -> return (CNil)
    ) 
    ++
    (
      p_par_ouv *> p_par_fer >>= fun (_, _) -> return (CUnit)
    )
  ) token_flux


let parse_miniml token_flux = run (map fst (parse_Expr *> p_eof)) token_flux;;
  
let rec print_solutions expr =
    match Solution.uncons expr with
    | None        -> ()
    | Some (p, q) ->
       begin
         Format.printf "Expression recognized: \n";
         Miniml_printer.print_expr Format.std_formatter p;
         Format.printf "\n";
         print_solutions q
       end;;
  
let test_parser_miniml () =
    let rec loop () =
      Format.printf "programme?@.";
      flush stdout;
      let l = read_line () in
      let f = read_miniml_tokens_from_string l in
      let progs = parse_miniml f in
      match Solution.uncons progs with
      | None        -> (Format.printf "** parsing failed ! **@."; loop ())
      | Some (p, q) ->
         begin
           print_solutions (Solution.cons p q);
           loop ()
        end
    in loop ();;