(* Tail-recursive and non tail-recursive verions of factorial *)

let rec factr (n : int) (acc: int): int = 
  if n = 0
  then acc
  else factr (n - 1) (n * acc)
;;

let rec fac (n : int) : int =
  if n = 0 then 1 else n * fac (n - 1)
;;

(* the two functions produce the same result for argument 10 *)
factr 10 1 = fac 10 ;;

