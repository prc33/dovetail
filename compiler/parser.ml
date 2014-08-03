(* Dovetail JCAM
 *
 * Copyright 2013      Peter Calvert <prc33@cam.ac.uk>
 *                     University of Cambridge Computer Laboratory            *)
 
(** JCAM IR Parser                                                            *)

open Jcam


module OptLevel = struct
  let value = try  int_of_string (Sys.getenv "DOVETAIL_OPTLEVEL")
              with _ -> 100

  let functional    attrs = if (value >= 1) then  CAttribute.Functional    :: attrs else attrs
  let closed        attrs = if (value >= 2) then  DAttribute.Closed        :: attrs else attrs
  let lower_bound i attrs = if (value >= 3) then (CAttribute.LowerBound i) :: attrs else attrs
  let upper_bound i attrs = if (value >= 3) then (CAttribute.UpperBound i) :: attrs else attrs
  let head          attrs = if (value >= 4) then  CAttribute.Head          :: attrs else attrs
end

let rec parse = parser
  (* Type alias definitions *)
  | [< 'Token.Id s;
       'Token.Symbol '=' ?? "expected '='";
       'Token.Kwd "type" ?? "expected 'type' keyword";
       t=parse_type ?? "expected type";
       p=parse >] ->
    { p with named_types = (s, t) :: p.named_types }
  (* Constant and 'extern' definitions *)
  | [< 'Token.GId s;
       'Token.Symbol '=' ?? "expected '='";
       stream >] ->
    begin parser
      (*| [< 'Token.Kwd "constant";
           e=parse_expr;
           p=parse >] ->
        {p with constants = (s, e) :: p.constants}*)
      | [< 'Token.Kwd "extern";
           'Token.Symbol '(' ?? "expected '('";
           ts=parse_typelist;
           'Token.Symbol ')' ?? "expected ')'";
           'Token.Symbol '-' ?? "expected '->'";
           'Token.Symbol '>' ?? "expected '->'";
           stream >] ->
        let (ktype,p) = begin parser
          | [< 'Token.Kwd "void";  p=parse >] -> (Type.Void, p)
          | [< 'Token.Kwd "async"; p=parse >] -> (Type.Async, p)
          | [< t=parse_type;       p=parse >] -> (Type.Return(t), p)
        end stream in {p with externs = (s, ts, ktype) :: p.externs}
      | [< >] -> raise (Stream.Error "global definitions are either 'constant' or 'extern'")
    end stream
  (* Definitions *)
  | [< 'Token.Kwd "definition";
       attrs=parse_def_attrs;
       'Token.Symbol '{' ?? "expected '{'";
       def=parse_def;
       'Token.Symbol '}' ?? "expected '}'";
       p=parse >] ->
    { p with definitions={ def with dattrs=attrs } :: p.definitions }
  (* Empty Program *)
  | [< >] -> { named_types=[]; externs=[]; definitions=[] }
  
and parse_type = parser
  (* Named Type *)
  | [< 'Token.Id s >] -> Type.Alias s
  (* Integer Type *)
  | [< 'Token.SizedType ('i', s) >] -> Type.Integer s
  (* Float Type *)
  | [< 'Token.SizedType ('f', s) >] -> Type.Float s
  (* Array Type *)
  | [< 'Token.Symbol '[';
       t=parse_type;
       'Token.Symbol ']' ?? "expected ']'" >] -> Type.Array t
  (* Structure Types *)
  | [< 'Token.Symbol '{';
       ts=parse_typelist;
       'Token.Symbol '}' ?? "expected '}'" >] -> Type.Struct ts
  (* Channel Types *)
  | [< 'Token.Symbol '(';
       ts=parse_typelist;
       'Token.Symbol ')' ?? "expected ')'" >] -> Type.Channel ts
       
and parse_typelist = parser
  | [< t=parse_type; stream >] ->
    begin parser
      | [< 'Token.Symbol ','; ts=parse_typelist >] -> t::ts
      | [< >] -> [t]
    end stream
  | [< >] -> []

and parse_def = parser
  (* Channel *)
  | [< 'Token.Kwd "channel";
       '(Token.GId _ | Token.Id _ as id) ?? "expected identifier";
       'Token.Symbol '(' ?? "expected '('";
       ts=parse_typelist;
       'Token.Symbol ')' ?? "expected ')'";
       attrs=parse_chan_attrs;
       def=parse_def >] ->
    { def with channels=(match id with
      | Token.GId s -> { name=s; constructor=true;  args=ts; cattrs=attrs }
      | Token.Id  s -> { name=s; constructor=false; args=ts; cattrs=attrs }
    )::def.channels }
  (* Transition *)
  | [< 'Token.Kwd "transition";
       pattern=parse_pattern;
       attrs=parse_transition_attrs;
       'Token.Symbol '{' ?? "expected '{'";
       blocks=parse_blocks None;
       'Token.Symbol '}' ?? "expected '}'";
       def=parse_def >] ->
    { def with transitions={ tid=create_id();
                             tattrs=attrs;
                             pattern=pattern;
                             blocks=blocks } :: def.transitions }
  (* Empty Definition *)
  | [< >] -> { did=create_id(); dattrs=[]; channels=[]; transitions=[] }
  
and parse_pattern = parser
  | [< 'Token.GId s;
       'Token.Symbol '(' ?? "expected '('";
       args=parse_args;
       'Token.Symbol ')' ?? "expected ')'"; >] -> [ (s, args) ]
  | [< chord=parse_chord >] -> chord
  
and parse_chord = parser
  | [< 'Token.Id s;
       'Token.Symbol '(' ?? "expected '('";
       args=parse_args;
       'Token.Symbol ')' ?? "expected ')'";
       c=parse_chord >] -> (s, args)::c
  | [< >] -> []
  
and parse_args = parser
  | [< t=parse_type; 'Token.Id v ?? "expected identifier"; stream >] ->
    begin parser
      | [< 'Token.Symbol ','; args=parse_args >] -> (t, v)::args
      | [< >] -> [(t, v)]
    end stream
  | [< >] -> []
  
and parse_blocks label = parser
  | [< block=parse_block; stream >] ->
    let lblock = { block with label=label } in
    begin parser
      | [< 'Token.Label l; blocks=parse_blocks (Some l) >] -> lblock :: blocks
      | [< >] -> [lblock]
    end stream
  
and parse_block = parser
  (* Phi and Assign *)
  | [< 'Token.Id v;
       'Token.Symbol '=' ?? "expected '='";
       stream >] ->
    begin parser
      | [< 'Token.Kwd "phi";
           t=parse_type;
           incoming=parse_incoming;
           block=parse_block >] ->               { block with phis=(v,t,incoming) :: block.phis }
      | [< e=parse_expr; block=parse_block >] -> { block with instrs=(Instruction.Assign (v, e)) :: block.instrs }
      | [< >] -> raise (Stream.Error "expected operation")
    end stream
  (* Store *)
  | [< 'Token.Kwd "store";
       t=parse_type ?? "expected type";
       a=parse_value;
       'Token.Symbol ',' ?? "expected ','";
       e=parse_value;
       'Token.Symbol ',' ?? "expected ','";
       i=parse_value;
       block=parse_block >] ->
    { block with instrs=(Instruction.Store (t, a, e, i)) :: block.instrs }
  (* Construct *)
  | [< 'Token.Kwd "construct";
       'Token.GId chan   ?? "expected constructor identifier";
       'Token.Symbol '(' ?? "expected '('";
       params=parse_params;
       'Token.Symbol ')' ?? "expected ')'";
       block=parse_block >] ->
    { block with instrs=(Instruction.Construct (chan, params)) :: block.instrs }
  (* Emit *)
  | [< 'Token.Kwd "emit";
       chan=parse_value;
       'Token.Symbol '(' ?? "expected '('";
       params=parse_params;
       'Token.Symbol ')' ?? "expected ')'";
       block=parse_block >] ->
    { block with instrs=(Instruction.Emit (chan, params)) :: block.instrs }
  (* Finish - terminates block *)
  | [< 'Token.Kwd "finish" >] -> { label=None;
                                   phis=[];
                                   instrs=[];
                                   terminator=Terminator.Finish }
  (* Branches - terminates block *)
  | [< 'Token.Kwd "br"; stream >] ->
    begin parser
      (* Unconditional *)
      | [< 'Token.Kwd "label";
           'Token.Id l >] ->     { label=None;
                                   phis=[];
                                   instrs=[];
                                   terminator=Terminator.Goto l }
      (* Conditional *)
      | [< v=parse_value;
           'Token.Symbol ',' ?? "expected ','";
           'Token.Kwd "label" ?? "expected label";
           'Token.Id a;
           'Token.Symbol ',' ?? "expected ','";
           'Token.Kwd "label" ?? "expected label";
           'Token.Id b >] ->     { label=None;
                                   phis=[];
                                   instrs=[];
                                   terminator=Terminator.Cond (v, a, b) }
    end stream
  | [< >] -> raise (Stream.Error "block ended unexpectedly.")

and parse_params = parser
  | [< t=parse_type; v=parse_value; stream >] ->
    begin parser
      | [< 'Token.Symbol ','; params=parse_params >] -> (t, v)::params
      | [< >] -> [(t, v)]
    end stream
  | [< >] -> []
  
and parse_expr = parser
  | [< 'Token.Kwd "add";
       t=parse_type ?? "expected type";
       a=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       b=parse_value ?? "expected value" >] -> Expr.Add (t, a, b)
  | [< 'Token.Kwd "sub";
       t=parse_type ?? "expected type";
       a=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       b=parse_value ?? "expected value" >] -> Expr.Sub (t, a, b)
  | [< 'Token.Kwd "mul";
       t=parse_type ?? "expected type";
       a=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       b=parse_value ?? "expected value" >] -> Expr.Mul (t, a, b)
  | [< 'Token.Kwd "div";
       t=parse_type ?? "expected type";
       a=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       b=parse_value ?? "expected value" >] -> Expr.Div (t, a, b)
  | [< 'Token.Kwd "cmp";
       o=parse_compare ?? "expected comparison operation";
       t=parse_type ?? "expected type";
       a=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       b=parse_value ?? "expected value" >] -> Expr.Compare (o, t, a, b)
  | [< 'Token.Kwd "array";
       l=parse_local;
       'Token.Symbol '[' ?? "expected '['";
       t=parse_type ?? "expected type";
       stream >] ->
    begin parser
      | [< 'Token.Symbol 'x';
           v=parse_value;
           'Token.Symbol ']' ?? "expected ']'" >] -> Expr.Array (t, l, Arrays.Length(v))
      | [< vs=parse_values;
           'Token.Symbol ']' ?? "expected ']'" >] -> Expr.Array (t, l, Arrays.InitList(vs))
    end stream
  | [< 'Token.Kwd "length";
       v=parse_value ?? "expected value" >] -> Expr.Length (v)
  | [< 'Token.Kwd "load";
       t=parse_type ?? "expected type";
       a=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       b=parse_value ?? "expected value" >] -> Expr.Load (t, a, b)
  | [< 'Token.Kwd "split";
       l=parse_local;
       t=parse_type ?? "expected type";
       a=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       b=parse_value ?? "expected value" >] -> Expr.Split (t, l, a, b)
  | [< 'Token.Kwd "merge";
       t=parse_type ?? "expected type";
       v=parse_value ?? "expected value" >] -> Expr.Merge (t, v)

and parse_local = parser
  | [< 'Token.Kwd "local" >] -> true
  | [< >] -> false

and parse_value = parser
  | [< 'Token.Id s >] -> Value.Var s
  | [< 'Token.Integer i >] -> Value.Integer i
  | [< 'Token.Float f >] -> Value.Float f
  | [< 'Token.Symbol '-'; stream >] ->
    begin parser
      | [< 'Token.Integer i >] -> Value.Integer (-i)
      | [< 'Token.Float f >] -> Value.Float (-.f)
    end stream;
  | [< 'Token.GId s >] -> Value.Constant s
  | [< 'Token.Kwd "null" >] -> Value.Null

and parse_values = parser
  | [< v=parse_value; stream >] ->
    begin parser
      | [< 'Token.Symbol ','; vs=parse_values >] -> v::vs
      | [< >] -> [v]
    end stream
  | [< >] -> []

and parse_incoming = parser
  | [< 'Token.Symbol '[';
       v=parse_value ?? "expected value";
       'Token.Symbol ',' ?? "expected ','";
       'Token.Id l ?? "expected label";
       'Token.Symbol ']' ?? "expected ']'";
       stream >] ->
    begin parser
      | [< 'Token.Symbol ','; incoming=parse_incoming >] -> (v,l)::incoming
      | [< >] -> [(v,l)]
    end stream
  | [< >] -> []

and parse_compare = parser
  | [< 'Token.Kwd "eq" >] -> Cmp.Eq
  | [< 'Token.Kwd "ne" >] -> Cmp.Ne
  | [< 'Token.Kwd "ugt" >] -> Cmp.UGt
  | [< 'Token.Kwd "uge" >] -> Cmp.UGe
  | [< 'Token.Kwd "ult" >] -> Cmp.ULt
  | [< 'Token.Kwd "ule" >] -> Cmp.ULe
  | [< 'Token.Kwd "sgt" >] -> Cmp.SGt
  | [< 'Token.Kwd "sge" >] -> Cmp.SGe
  | [< 'Token.Kwd "slt" >] -> Cmp.SLt
  | [< 'Token.Kwd "sle" >] -> Cmp.SLe

and parse_def_attrs = parser
  | [< 'Token.Kwd "inline"; attrs=parse_def_attrs >] -> DAttribute.Inline :: attrs
  | [< 'Token.Kwd "closed"; attrs=parse_def_attrs >] -> OptLevel.closed attrs
  | [< 'Token.Kwd "singleton"; attrs=parse_def_attrs >] -> DAttribute.Singleton :: attrs
  | [< >] -> []
  
and parse_chan_attrs = parser
  | [< 'Token.Kwd "functional"; attrs=parse_chan_attrs >] -> OptLevel.functional attrs
  | [< 'Token.Kwd "lower_bound";
       'Token.Symbol '(' ?? "expected '('";
       'Token.Integer i      ?? "expected integer";
       'Token.Symbol ')' ?? "expected ')'";
       attrs=parse_chan_attrs >] -> OptLevel.lower_bound i attrs
  | [< 'Token.Kwd "upper_bound";
       'Token.Symbol '(' ?? "expected '('";
       'Token.Integer i      ?? "expected integer";
       'Token.Symbol ')' ?? "expected ')'";
       attrs=parse_chan_attrs >] -> OptLevel.upper_bound i attrs
  | [< 'Token.Kwd "cell"; attrs=parse_chan_attrs >] -> OptLevel.upper_bound 1 attrs
  | [< 'Token.Kwd "mem"; attrs=parse_chan_attrs >] -> OptLevel.lower_bound 1 (OptLevel.upper_bound 1 (OptLevel.head attrs))
  | [< >] -> []
  
and parse_transition_attrs = parser
  | [< 'Token.Kwd "inline"; attrs=parse_transition_attrs >] -> TAttribute.Inline :: attrs
  | [< >] -> []
