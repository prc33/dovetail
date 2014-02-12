open Jcam

let return_type = function
  | Expr.Add(t,_,_) -> t
  | Expr.Sub(t,_,_) -> t
  | Expr.Compare(_,t,_,_) -> t
