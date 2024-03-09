(* Higher-order version of Ackermann function with function iteration *)

(* Can be evaluated in Caml interpreter 
   but probably produces "incorrect" Python code. Why?  *)

let rec succ (n : int) : int = n + 1
;;

let rec iter (f : int -> int) (n : int) : int = 
  if n = 0 then (f 1) else f(iter f (n - 1))
;;

let rec ack (m : int) : int -> int =
  if m = 0 then succ else iter (ack (m - 1))
;;

ack 3 2;;
