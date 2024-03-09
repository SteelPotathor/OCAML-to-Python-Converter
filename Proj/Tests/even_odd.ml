(* Attention, this code is not valid Ocaml syntax.
   The mutually recursive functions have to be combined with and
 *)

let rec even (n : int) : bool = (n = 0) || odd (n - 1) ;;
let rec odd (n : int) : bool = (n <> 0) && even (n - 1) ;;
even 7 ;;

