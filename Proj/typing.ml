open Lang

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

exception NotFound;;

let tp_const = function
    | BoolV _ -> BoolT
    | IntV _ -> IntT;;

let search_tp_localvar v env = 
    let rec aux = function
        | (a, b)::c -> if v = a then b else aux c
        | _ -> raise NotFound
in aux env;; 

let search_tp_funbind v env = 
    let rec aux = function
        | (a, b)::c -> if v = a then fst b else aux c
        | _ -> raise NotFound
in aux env;; 

let type_creation = function 
    | FPdecl(t_retour, _, (Vardecl(a, b)::c)) -> 
        let rec aux = function 
            | (Vardecl(a, b)) -> b
    		| (Vardecl(a, b)::c) -> FunT(b, aux c) 
    in FunT(aux c, t_retour)
    | FPdecl(t_retour, _, _) -> t_retour;;

let tp_var env v = try type_creation (search_tp_var v env.localvar) with | NotFound -> try type_creation (search_tp_funbind v env.funbind) with | NotFound -> failwith "echec de typage, la variable n'est pas dans l'environnement";;

let rec function_type_correct tf tparam = match tparam with 
    | (a::b) -> match tf with | FunT(t1, t2) -> if t1 = a then function_type_correct t2 b else failwith "erreur le type du paramètre et le type de la fonction ne correspondent pas"
                              | _ -> failwith "erreur application d'une non fonction"
    | [] -> tf;; 

let rec tp_expr env = function
    | Const c -> tp_const c 
    | VarE v -> tp_var env v
    | BinOp(b, e1, e2) -> match b with | BArith b -> let t1 = tp_expr env e1 in if t1 = IntT && t1 = tp_expr env e2 then t1 else failwith "echec de typage, les opérations arithmétiques se font sur des int" 
                                       | BCompar b ->  if tp_expr env e1 = tp_expr env e2 then BoolT else failwith "echec de typage, les comparaisons doivent être de même type"
                                       | BLogic b -> let t1 = tp_expr env e1 in if t1 = BoolT && t1 = tp_expr env e2 then BoolT else failwith "echec de typage, les opérations logiques se font sur des bool"
    | IfThenElse(e1, e2, e3) -> (if tp_expr env e1 = BoolT then let t2 = tp_expr env e2 in (if t2 = tp_expr env e3 then t2 else failwith "echec de typage, then et else n'ont pas le même type") else failwith "echec de typage, if n'est pas un booléen")
    | CallE(a::b) -> let tf = tp_expr env a and tparam = List.map (tp_expr env) b in function_type_correct tf tparam
    | CallE(_) -> failwith "erreur" (* est censée ne jamais être vide *);;


let fun_bind fdfs = List.map (fun (Fundefn(fd, _)) -> (name_of_fpdecl fd, fd) ) fdfs;;

let environment_initial fdfs = {localvar = []; funbind = fun_bind fdfs};;

let tp_prog (Prog (fdfs, e)) = let initial = environment_initial fdfs in List.for_all (fun fd -> tp_defn initial fd) fdfs ;;

(* ajoute une fonction (déclaration) à l'env et verifie son expression renvoie un bool *)
let tp_fdefn env f = 
    let envlocal = env{localvar = declarations_de f}

(f, tp_expr env f)::env.localvar;;

(* si verif incorrecte => exception *)




(* recuperer toutes les vraibales d'une epxr et vérifier que f n'est pas présent 

header => juste virer le type de retour

transformation => rien bouger si recursif non terminale

tout tester, ne pas prendre de risques *)
