open Lang

type result =
    Val of value
  | Closure of fpdefn * (result list)

(* environment *)
type state = (vname * result) list

let eval_barith b v1 v2 boo = match v1, v2 with | Val(IntV i1), Val(IntV i2) -> if boo && i2 = 0 then failwith "Division par zéro" else Val(IntV(b v1 v2)) 
                                                | _ -> failwith "L'opération arithmétique s'effectue entre deux entiers";;

let eval_blogic b v1 v2 = match v1, v2 with | Val(BoolV b1), Val(BoolV b2) -> Val(BoolV(b b1 b2))
                                            | _ -> failwith "L'opération logique s'effectue entre deux booléens";;

let eval_bcompar b v1 v2 = match v1, v2 with | Val(IntV i1), Val(IntV i2) -> Val(BoolV(b i1 i2))
                                             | Val(BoolV b1), Val(BoolV b2) -> Val(BoolV(b b1 b2))
                                             | _ -> failwith "L'opération de comparaison s'effectue entre deux entiers ou deux booléens";;

let rec eval_expr env = function 
        | Const v -> Val v
        | VarE v -> (match List.assoc_opt v env with | Some s -> s | _ -> failwith "Variable non présente dans l'environnement")
        | BinOp (b, e1, e2) -> let v1 = eval_expr env e1 and v2 = eval_expr env e2 in (match b with | BArith BAadd -> eval_barith (+) v1 v2 false
                                                                                                    | BArith BAsub -> eval_barith (-) v1 v2 false
                                                                                                    | BArith BAmul -> eval_barith ( * ) v1 v2 false
                                                                                                    | BArith BAdiv -> eval_barith (/) v1 v2 true
                                                                                                    | BArith BAmod -> eval_barith (mod) v1 v2 false
                                                                                                    | BLogic BLand -> eval_blogic (&&) v1 v2
                                                                                                    | BLogic BLor -> eval_blogic (||) v1 v2
                                                                                                    | BCompar BCeq -> eval_bcompar (=) v1 v2
                                                                                                    | BCompar BCge -> eval_bcompar (>=) v1 v2
                                                                                                    | BCompar BCgt -> eval_bcompar (>) v1 v2
                                                                                                    | BCompar BCle -> eval_bcompar (<=) v1 v2
                                                                                                    | BCompar BClt -> eval_bcompar (<) v1 v2
                                                                                                    | BCompar BCne -> eval_bcompar (!=) v1 v2)
        | IfThenElse(e1, e2, e3) -> (match eval_expr env e1 with | Val(BoolV true) -> eval_expr env e2
                                                             | Val(BoolV false) -> eval_expr env e3
                                                             | _ -> failwith "La condition n'est pas un booléen")
        | CallE(a::b) -> let eval_f = eval_expr env a and eval_param = List.map (eval_expr env) b in 
            (match eval_f with | Closure(fp, _) -> (match fp with | Fundefn(fpdecl, e) -> let rec apply_params varlist eval_param envlocal used = 
                                                                                            (match varlist, eval_param with | [], [] -> eval_expr envlocal e
                                                                                            | (Vardecl(name, _)::b), x::y -> apply_params b y ((name, x)::envlocal)
                                                                                            | (Vardecl(name, _)::b), _ -> Closure(Fundefn(fpdecl, e), List.rev used)
                                                                                            | _ -> failwith "Trop de paramètres appliqués à la fonction")
                                                                                          in apply_params (params_of_fpdecl fpdecl) eval_param env []
                                                                  | Procdefn _ -> failwith "Une procédure ne peut être appelée")
                               | _ -> failwith "Application à une non fonction") (* This should never happen if the program is correctly typed *);;

let eval_fpdefn env = function
        | Fundefn(FPdecl(tp, name, l), e) -> (name, (eval_expr env e)) :: env
        | Procdefn _ -> failwith "Une procédure ne peut être appelée" (* to implement ? *);;

let eval_prog (Prog (fdfs, e)) = let env = List.fold_right (fun x f -> eval_fpdefn x f) [] fdfs in Val(eval_expr env e);;
