type 'a logic = Var of GT.int | Value of 'a * string [@@deriving gt { show } ]

type 'a megalist = Nil_ | Cons of 'a * 'a megalist  [@@deriving gt { show } ]
let () =
  print_endline @@ GT.(show logic @@ show megalist @@ show int)
                     (Value (Cons (5, Nil),"a") )
(* type ident = [`Var of string] [@@ deriving gt] *)

(* class ['v] ident_eval = object *)
(*   inherit [string -> 'v, 'v] ident [@@deriving inher] *)
(*   method c_Var s _ x = s x *)
(* end *)

(* type 'a arith = [ `Add of 'a * 'a | `Sub of 'a * 'a]  [@@ deriving gt] *)

(* class ['a, 'b] arith_eval = object *)
(*   inherit ['a, 'b, int, 'b, int] arith [@@deriving inher] *)
(*   method c_Add inh _ x y = x.GT.fx inh + y.GT.fx inh *)
(*   method c_Sub inh _ x y = x.GT.fx inh - y.GT.fx inh *)
(* end *)

(* type 'a expr = [ ident | 'a arith ]  [@@ deriving gt] *)

(* class ['a] expr_eval = object(this) *)
(*   inherit ['a, string->int, int, string->int, int] expr [@@deriving inher] *)
(*   inherit [int] ident_eval *)
(*   inherit ['a, string -> int] arith_eval *)
(* end *)

(* let _ = *)
(*   let rec eval f x = GT.transform(expr) eval (new expr_eval) f x in *)
(*   Printf.printf "%d\n" (eval (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y"))) *)
