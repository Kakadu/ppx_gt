(* type xxx = XXX of int [@@deriving gt {show} ] *)
(* let () = *)
(*   print_endline @@ GT.(show xxx) (XXX 666) *)

type 'a logic = Var of int | Value of 'a [@@deriving gt { show } ]

type nat = N | S of nat logic [@@deriving gt { show } ]
type 'a llist = Nil | Cons of 'a logic * 'a llist logic [@@deriving gt { show } ]
type ('a, 'self) tree = Nil | Node of 'a * 'self * 'self [@@deriving gt { show } ]
type expr  = I | A of expr logic * expr logic | M of expr logic * expr logic
  [@@deriving gt { show } ]

type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic
  [@@deriving gt { show } ]
type typ = V of string logic | Arr of typ logic * typ logic [@@deriving gt { show } ]

type ('var, 'self) alam = X of 'var | App of 'self * 'self | Abs of 'var * 'self
  [@@deriving gt { show } ]

type ('var, 'self) atyp = V of 'var | Arr of 'self * 'self [@@deriving gt { show } ]


(* type token = Id | Add | Mul [@@deriving gt { show } ] *)
(* type expr  = I | A of expr logic * expr logic | M of expr logic * expr logic [@@deriving gt { show } ] *)

(* type nat = O | S of nat logic [@@deriving gt { show } ] *)
(* type 'a nat2 = *)
(*   | JustAlpha of 'a *)
(*   | Myself of 'a nat2 *)
(*   | LogicMyself of 'a nat2 logic *)
(*   | LogicLogicMyself of 'a nat2 logic logic [@@deriving gt { show } ] *)


(* type 'a megalist = Nil | Cons of 'a * 'a megalist  [@@deriving gt { show } ] *)

(* let () = *)
(*   print_endline @@ GT.(show logic @@ show megalist @@ show int) *)
(*                      (Value (Cons (5, Nil), 18) ) *)

(* type 'a logic2 = Var of int |  Value of 'a [@@deriving gt { show } ] *)

(* let () = *)
(*   print_endline @@ GT.(show logic2 @@ show int) (Value 20); *)
(*   print_endline @@ GT.(show logic2 @@ show string) (Value "asdf"); *)
(*   () *)

(* type t = N | A of t logic [@@deriving gt { show } ] *)


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
