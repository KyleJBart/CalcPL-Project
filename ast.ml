type bop = 
  | Add
  | Mult
  | Sub (* NEW *)
  | Div (* NEW *)
  | FAdd (* NEW *)
  | FMult (* NEW *)
  | FSub (* NEW *)
  | FDiv (* NEW *)
  | Leq
  | Geq (* NEW *)

(** [typ] represents the type of an expression. *)
type typ =
  | TInt
  | TBool
  | TFloat (* NEW *)

type expr = 
| Var of string
| Int of int
| Bool of bool
| Binop of bop * expr * expr
| Float of float
| Let of string * typ * expr * expr
| If of expr * expr * expr
