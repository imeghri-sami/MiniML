open Miniml_types
open Miniml_printer
open Lazyflux

exception Typing_Exception of string

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

    val init : unit -> env

    val lookup : env -> expr -> TypeVariable.t typ
    
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
      let rec list_to_flux l = 
        match l with | [] -> None | t::q -> Some(t, q)  
      in Flux.unfold list_to_flux initial_typ_list
  
    let rec lookup env_flux expression = match Flux.uncons env_flux with
        | None -> raise (Typing_Exception("Unbound value"))
        | Some(exprTyp, env_flux') -> 
          if expression = (fst exprTyp) then (snd exprTyp) else (lookup env_flux' expression)
    
    let extend env_flux expression etyp = Flux.cons (expression, etyp) env_flux
  end

(*module type EquationSPEC = 
  sig

  end

module EquationSet : EquationSPEC = 
  struct

  end*)