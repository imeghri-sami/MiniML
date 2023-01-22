 
open Miniml_types
open Miniml_parser
open Miniml_typer
open Miniml_printer
(*let expr = ELet("x", EConstant(CEntier 2), EConstant(CEntier 3))*)

(*let () = Format.printf "Expression : %a\n" print_expr expr*)

(*Cette fonction teste si deux types sont egaux*)
let type_equals t_given t_expected = (t_given = t_expected)

(*****************Test des fonctions init, extend, lookup de Environnement*****************)
let env = Environment.init()
let expr = ELet("x", EConstant(CEntier 2), EApply(EApply(EBinop(PLUS), EIdent("x")), EConstant(CEntier 3)))
let env = Environment.extend env expr TInt
let typa = (Environment.lookup env expr)

let%test _ = type_equals typa TInt


(*****************Test de la fonction add de EquationSet*****************)
let x = TypeVariable.fraiche ()
let y = TypeVariable.fraiche ()
let eqs = EquationSet.empty
(* add the equation x = y to the equation set *)
let eqs = EquationSet.add (x, y) eqs

let%test _ = let l = flux_as_list eqs in List.length l == 1
let z = TypeVariable.fraiche ()
let eqs = (EquationSet.add (x, z) eqs)

let%test _ = let l = flux_as_list eqs in List.length l == 2

(*****************Test de la fonction substitute de EquationSet*****************)
let t = TFun(TVar(x), TVar(x))
let t' = EquationSet.substitute x (TVar(y)) t
let t_expected = TFun(TVar(y), TVar(y))

let%test _ = type_equals t' t_expected

(*Un autre test*)
let t = TList(TVar(x))
let t' = EquationSet.substitute x TInt t
let t_expected = TList(TInt)

let%test _ = type_equals t' t_expected


(*****************Test de la fonction occurs de EquationSet*****************)

let t = TFun(TVar(x), TVar(x))
let result = EquationSet.occurs x t

let%test _ = result
(* type 'a -> 'b -> 'c list *)
let t = TFun(TVar(x), TFun(TVar(y), TList(TVar(z))))
(* check if variable y occurs in type t *)
let result1 = EquationSet.occurs y t
let result2 = EquationSet.occurs x t
let result3 = EquationSet.occurs z t

let%test _ = result1 && result2 && result3


(*****************Test de la fonction collect_constraints*****************)
let expr = ELet("x", EConstant(CEntier(5)), EConstant(CBooleen(true)))

let env = Environment.init()

let (typr, _, eqs) = collect_constraints expr env

let%test _ = List.length (flux_as_list eqs) = 0 

(*************************** Test de Normalize****************************)

let expression = EFun("x", EApply(EBinop(PLUS), EIdent "x"))

let env = Environment.init ()

(* Collecting constraints from the expression *)
let (typ, env, eq_flux) = collect_constraints expression env

(* Normalizing the collected equations *)
let final_typ = normalize eq_flux typ

let%test _ = type_equals final_typ (TFun(TInt, TFun(TInt, TInt)))