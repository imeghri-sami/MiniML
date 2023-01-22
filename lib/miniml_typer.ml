open Miniml_types
open Miniml_printer
open Lazyflux

exception Typing_Exception of string
exception NotImplemented of string

(* signature minimale pour définir des variables *)
module type VariableSpec =
  sig
    (* type abstrait des variables      *)
    type t

    (* création d'une variable fraîche  *)
    val fraiche : unit -> t

    (* fonctions de comparaison         *)
    (* permet de définir des conteneurs *)
    (* (hash-table, etc) de variables   *)
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int

    (* fonction d'affichage             *)
    (* on utilise Format.std_formatter  *)
    (* comme premier paramètre          *)
    (* pour la sortie standard          *) 
    val fprintf : Format.formatter -> t -> unit
  end

(* implantation de la spécification     *)
module TypeVariable : VariableSpec =
  struct
    type t = int

    let fraiche =
      let cpt = ref 0 in
      (fun () -> incr cpt; !cpt)

    let compare a b = a - b
    let equal a b = a = b
    let hash a = Hashtbl.hash a

    let fprintf fmt a = Format.fprintf fmt "t{%d}" a
  end

module type EnvironmentSPEC = 
  sig
    type env

    (* 
      fonction qui initialise un flot par les types des operations standard
       du langage.
       parametres : unit
       retourne : un flot qui contient des couples de la forme (expression, type)
       où l' expression est exprimée par les types expr de Miniml_types et le type
       est le type de l'expression  
    *)
    val init : unit -> env

    (*
      fonction qui permet de récupere le type d'une expression à partie d'un environnement
      parametres : 
        - env : l'environnement sur lequel on fait la recherche
        - expr : l'expression qui cherche à trouver son type dans l'environnement env
      retourne : le type de l'expressison dans l'environnement
      exception : Typing_Exception : Si l'expression n'existe pas dans l'environnement
    *)
    val lookup : env -> expr -> TypeVariable.t typ
    
    (*
      fonction qui permet d'ajouter une expression dans l'environnement
      parametres : 
        - env : l'environnement dans lequel on va ajouter l'expression
        - expr : l'expression qu'on veut ajouter
        - typ : le type de l'expression à ajouter
      retourne : env' = {(expr, type)} U env un nouveau environnement avec
      l'expression ajoutée 
    *)
    val extend : env -> expr -> TypeVariable.t typ -> env
  end 
       
module Environment : EnvironmentSPEC = 
  struct 
    (* Définition du type environnement *)
    type env =  (expr * TypeVariable.t typ) Flux.t

    open Lazyflux
    let init () = 
      let typeA = TypeVariable.fraiche () and typeB = TypeVariable.fraiche () in 
      let initial_typ_list = [
        (*'a list −> 'a list −> 'a list*)
        (EBinop(CONCAT), TFun(TList(TVar(typeA)), TFun(TList(TVar(typeA)), TList(TVar(typeA)))));
        (*'a −> 'a list −> 'a list *)
        (EBinop(CONS), TFun(TVar(typeA), TFun(TList(TVar(typeA)), TList(TVar(typeA)))));
        (*'a −> 'b −> 'a * 'b Rq : la grammair ne contient pas une regle de construction des paires*) 
        (EBinop(VIRG), TFun(TVar(typeA), TFun(TVar(typeB), TVar(typeA))));
        (*int −> int −> int *)
        (EBinop(PLUS), TFun(TInt, TFun(TInt, TInt)));
        (EBinop(MOINS), TFun(TInt, TFun(TInt, TInt)));
        (EBinop(MULT), TFun(TInt, TFun(TInt, TInt)));
        (EBinop(DIV), TFun(TInt, TFun(TInt, TInt)));
        (* 'a −> 'a −> bool*)
        (EBinop(EQU), TFun(TVar(typeA), TFun(TVar(typeA), TBool)));
        (EBinop(NOTEQ), TFun(TVar(typeA), TFun(TVar(typeA), TBool)));
        (EBinop(INFEQ), TFun(TVar(typeA), TFun(TVar(typeA), TBool)));
        (EBinop(INF), TFun(TVar(typeA), TFun(TVar(typeA), TBool)));
        (EBinop(SUPEQ), TFun(TVar(typeA), TFun(TVar(typeA), TBool)));
        (EBinop(SUP), TFun(TVar(typeA), TFun(TVar(typeA), TBool)));
        (*bool −> bool −> bool*)
        (EBinop(AND), TFun(TBool, TFun(TBool, TBool)));
        (EBinop(OR), TFun(TBool, TFun(TBool, TBool)));
        (*bool −> bool*)
        (EIdent("not"), TFun(TBool, TBool));
        (*'a * 'b −> 'a *)
        (EIdent("fst"), TFun(TProd(TVar(typeA), TVar(typeB)), TVar(typeA))); 
        (*'a * 'b −> 'b *)
        (EIdent("snd"), TFun(TProd(TVar(typeA), TVar(typeB)), TVar(typeB)));
        (*'a list −> 'a *)
        (EIdent("hd"), TFun(TList(TVar(typeA)), TVar(typeA)));
        (*'a list −> 'a list *)
        (EIdent("tl"), TFun(TList(TVar(typeA)), TList(TVar(typeA))))
      ] in 
      let list_to_flux l = 
        match l with | [] -> None | t::q -> Some(t, q)  
      in Flux.unfold list_to_flux initial_typ_list
  
    let rec lookup env_flux expression = match Flux.uncons env_flux with
        | None -> raise (Typing_Exception("Unbound value"))
        | Some(exprTyp, env_flux') -> 
          if expression = (fst exprTyp) then (snd exprTyp) else (lookup env_flux' expression)
    
    let extend env_flux expression etyp = Flux.cons (expression, etyp) env_flux
  end



module type EquationSPEC = 
  sig
    (* 
      Le type d'un ensemble des equations, chaque élement est sous
      la forme de (t1, t2) où t1 est la partie gauche de l'equation et t2 
      sa partie droite. l'ensemble des equation est representé par un flot 
    *)
    type t
    (*
      Creer un flot vide   
    *)
    val empty : t
    (*
      fonction qui permet d'ajouter une équation dans un enemble
      des equations
      parametres :
        - equation : l'equation à ajouter
        - equation_flux : le flot des équations dans lequel on 
        veut ajouter
      retourne : un nouveau flot avec l'equation ajoutée 
    *)
    val add : 'a -> 'a Flux.t -> 'a Flux.t
    (*
      fonction qui remplace un type de variable par un type
      dans un type d expression
      parametres : 
        - x : le type de variable qu on veut remplacer
        - t : le nouveau type
        - texpr : le type d expression
      retourne : le nouvrau type d expression avec le type remplacé
      exception : NotImplemented si le type d expression contient une type TProd
    *)
    val substitute : TypeVariable.t -> TypeVariable.t typ -> TypeVariable.t typ -> TypeVariable.t typ
    (*
      fonction qui applique une substition sur l ensemble des equations génerées
      parametres : 
        - x : le type de variable qu on veut remplacer
        - t : le nouveau type
        - equation_flux : l'ensemble des equations
      retourne : un nouveau ensemble d equations avec le type remplacé
    *)
    val apply_subst : TypeVariable.t -> TypeVariable.t typ -> t -> t
    (*
      fonction qui permet de verifier qu'un type de variable existe 
      dans une expression de type
      parametres : 
        - t : le type qu on veut verifier son existance
        - texpr : l expression des types
      retourne : vrai si le type de variable existe dans l'expression 
      des types, faux sinon
    *)
    val occurs : TypeVariable.t -> TypeVariable.t typ -> bool 
    (* fonction pour concatener deux flots d equation *)
    val ( ++ ) :  t -> t -> t 
  end

module EquationSet = 
  struct
    type t =  (TypeVariable.t typ * TypeVariable.t typ) Flux.t
    
    let empty = Flux.vide;;

    let add equation equation_flux = Flux.cons equation equation_flux

    let rec substitute x t = function 
      | TUnit | TBool | TInt as t' ->  t'
      | TFun(a, b) -> TFun ((substitute x t a), (substitute x t b))
      | TList(a) -> TList( (substitute x t a) )
      | TVar(a) as t' -> if TypeVariable.equal a x then t else t' 
      | TProd(_, _) -> raise (NotImplemented "EProd n existe pas dans la grammaire du langage")

    let rec apply_subst x t equation_flux = match Flux.uncons equation_flux with
      | None -> Flux.vide
      | Some ( (t1, t2), q) -> add ((substitute x t t1), (substitute x t t2)) (apply_subst x t q)
    
    let rec occurs t1 t2 = match t2 with
      | TProd(_, _) -> raise (NotImplemented "EProd n existe pas dans la grammaire du langage")
      | TFun(a, b) -> occurs t1 a || occurs t1 b
      | TList(a) -> occurs t1 a
      | TVar(a) -> TypeVariable.equal t1 a
      | _ -> false

    let (++) eq1 eq2 = Flux.(eq1 ++ eq2) 
  end
(* 
  Fonctoin qui permet de génerer les equations pour une expression
  parametres : 
    - expression : l 'expression surlaquelle on genere les equations
    - environment : l'environnement initial pour Miniml   
  retourne : un tuple de (type_fraiche, environment, equation_flux)
    - type_fraiche : le type fraiche a calculer de l expression 
    - environment : l'environnement (utilisé dans le cas d'une expression letrec)
    - equation_flux : les equations générées 
*)
let rec collect_constraints = fun expression environment  -> 
  match expression with
  | EConstant(c)          -> 
    (
     ( match c with CBooleen(_) -> TBool | CEntier (_) -> TInt | CUnit -> TUnit | CNil -> TList (TVar(TypeVariable.fraiche ())) ),
     environment,
     EquationSet.empty
    )
  | EIdent(_)             -> (Environment.lookup environment expression), environment, EquationSet.empty
  (* les EProd n existe pas dans la grammaire du langage *)
  | EProd(_ ,_)           -> raise (NotImplemented "EProd n existe pas dans la grammaire du langage")
  | ECons(e1, e2)         -> let (typ1, _, eq1) = (collect_constraints e1 environment )
    in let (typ2, _, eq2) = (collect_constraints e2 environment)
    in let equations = EquationSet.(eq1 ++ eq2)
    in typ2, environment, ( EquationSet.add (typ2, (TList(typ1))) equations )  
  | EFun(id, body)        -> let typ_alpha = TVar(TypeVariable.fraiche ()) 
    in let (t_body, _, eq_body) = (collect_constraints body (Environment.extend environment (EIdent id) typ_alpha)) 
    in (TFun (typ_alpha, t_body), environment, eq_body)
  | EIf(e1, e2, e3)       -> let (typ1, _, eq1) = (collect_constraints e1 environment)
    in let (typ2, _, eq2) = (collect_constraints e2 environment)
    in let (typ3, _, eq3) = (collect_constraints e3 environment) 
    in let equations = EquationSet.( (eq1 ++ eq2) ++ eq3 )
    in typ2, environment, (EquationSet.add (typ2, typ3) (EquationSet.add (typ1, TBool) equations))
  | EApply(e1, e2)       -> let (typ1, _, eq1) = collect_constraints e1 environment
    in let (typ2, _, eq2) = collect_constraints e2 environment
    in let typ_alpha = TVar(TypeVariable.fraiche ()) 
    and equations = EquationSet.(eq1 ++ eq2) 
    in (typ_alpha, environment, (EquationSet.add (typ1, TFun(typ2, typ_alpha)) equations))
  | EBinop(_)           -> (Environment.lookup environment expression), environment, EquationSet.empty 
  | ELet(id, e1, e2)    -> let (typ1, _, eq1) = collect_constraints e1 environment
    in let env2 = Environment.extend environment (EIdent(id)) typ1
    in let (typ2, _, eq2) = collect_constraints e2 env2
    in let equations = EquationSet.(eq1 ++ eq2)
    in (typ2, env2, equations) 
  | ELetrec(id, e1, e2) -> let typ_alpha = TVar(TypeVariable.fraiche ()) 
    in let (typ1, env2, eq1) = collect_constraints e1 (Environment.extend environment (EIdent id) typ_alpha)
    in let (typ2, _, eq2) = collect_constraints e2 env2
    in let equations = EquationSet.(eq1 ++ eq2)
    in (typ2, env2, (EquationSet.add (typ_alpha, typ1) equations))
  | EExprPar(e)         -> collect_constraints e environment 

(*
    fonction qui permet de determiner le type de 't' à partir
    d'un ensemble des equations en utilisant les regles de normalisation.
    parametres : 
      - equation_flux : la liste des equations générées
      - t : le type fraiche qu'on veut determiner son type
    retourne : le resultat de la résolution
*)
let rec normalize = fun equation_flux t -> 
  match Flux.uncons equation_flux with
    | Some(equation, q) -> begin match equation with
      | TInt, TInt | TBool, TBool | TUnit, TUnit -> normalize q t
      | TList(a), TList(b) -> normalize (EquationSet.add (a,b) q) t
      | TFun(a, b), TFun(c, d) -> normalize (EquationSet.add (a,c) (EquationSet.add (b,d) q)) t
      | TProd(_, _), TProd(_,_) -> raise(Typing_Exception "Unregonized type")
      | TVar(a), TVar(b) -> if TypeVariable.equal a b then normalize q t 
        else normalize (EquationSet.apply_subst a (snd equation) q) (EquationSet.substitute a (snd equation) t)
      | TVar(a), b -> if EquationSet.occurs a b then raise (Typing_Exception "Typing error !")
        else normalize (EquationSet.apply_subst a (snd equation) q) (EquationSet.substitute a (snd equation) t)
      | _, TVar(_) -> normalize (EquationSet.add (snd equation, fst equation) q) t
      | _, _ -> raise (Typing_Exception "Unbound value") end
    | None              -> t     

let infere_type_of expression = 
  let (t, _, equations) = collect_constraints expression (Environment.init ())
  in ((normalize equations t))

open Miniml_parser;;

(*
  fonction qui permet de convertir un flot a une liste
*)
let rec flux_as_list f = match Flux.uncons f with 
  | None -> []
  | Some(v, q) -> v :: flux_as_list q


let rec print_solutions expr =
  match Solution.uncons expr with
  | None        -> ()
  | Some (p, q) ->
    begin
      Miniml_printer.print_expr Format.std_formatter p;
      Format.printf "\n Type ---> ";
      (
        try 
          let result_typ = infere_type_of p
          in Miniml_printer.print_typ TypeVariable.fprintf Format.std_formatter result_typ;
          Format.printf "\n"; 
        with 
          | Typing_Exception(msg) -> Format.printf ": Typing error (%s)" msg;
          | NotImplemented(msg) -> Format.printf ": Not implemented (%s)" msg;
      );
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