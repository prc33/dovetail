open Jcam

let return_type = function
  | Expr.Add(t,_,_) -> t
  | Expr.Sub(t,_,_) -> t
  | Expr.Mul(t,_,_) -> t
  | Expr.Div(t,_,_) -> t
  | Expr.Compare(_,t,_,_) -> Type.Integer(1)
  | Expr.Array(t,_,_) -> Type.Array(t)
  | Expr.Length(_) -> Type.Integer(32)
  | Expr.Load(Type.Array(t),_,_) -> t
  | Expr.Split(t,_,_,_) -> Type.Array(t)
  | Expr.Merge(t,_) -> t

(* TODO: check type aliases acyclic *)
