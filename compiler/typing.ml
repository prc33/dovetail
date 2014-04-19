open Jcam

let return_type = function
  | Expr.Add(t,_,_) -> t
  | Expr.Sub(t,_,_) -> t
  | Expr.Compare(_,t,_,_) -> t
  | Expr.Array(t,_,_) -> Type.Array(t)
  | Expr.Length(_) -> Type.Integer(32)
  | Expr.Load(Type.Array(t),_,_) -> t
  | Expr.Split(t,_,_,_) -> Type.Array(t)
  | Expr.Merge(t,_) -> t
