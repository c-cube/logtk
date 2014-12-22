(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBBTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BBT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBBTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BBT NOT LIMITED TO, PROCUREMENT OF SUBSTITBTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OBT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(* {1 LogtkType inference} *)

(** Reference:
    https://en.wikipedia.org/wiki/Hindley-Milner
*)

module Ty = LogtkType
module S = LogtkSubsts
module Sym = LogtkSymbol
module Loc = LogtkParseLocation
module Err = CCError

let prof_infer = LogtkUtil.mk_profiler "LogtkTypeInference.infer"
let section = LogtkUtil.Section.(make ~parent:logtk "ty_infer")

type 'a or_error = [`Error of string | `Ok of 'a]

(** {2 Default LogtkTypes} *)

type default_types = {
  default_i : LogtkType.t;
  default_prop : LogtkType.t;
  default_int : LogtkType.t;
  default_rat : LogtkType.t;
}

let tptp_default = {
  default_i = LogtkType.TPTP.i;
  default_prop = LogtkType.TPTP.o;
  default_int = LogtkType.TPTP.int;
  default_rat = LogtkType.TPTP.rat;
}

(** {2 Typing context}

The scope maintained by the typing context starts at 1.
Scope 0 should be used for ground types.
*)

module Ctx = struct
  type t = {
    default : default_types;        (* default types *)
    mutable signature : LogtkSignature.t;(* symbol -> type *)
    mutable subst : S.t;            (* variable bindings *)
    mutable vars_ty : LogtkType.t list;  (* type variables of variables *)
    mutable const_ty : LogtkType.t list; (* type variables of constants *)
    mutable locs : Loc.t list; (* stack of locations *)
    renaming : LogtkSubsts.Renaming.t;
    symbols : LogtkType.t LogtkSymbol.Tbl.t;  (* symbol -> instantiated type *)
    tyvars : LogtkType.Conv.ctx;         (* type variable -> number *)
    vars : (string, (int * LogtkType.t)) Hashtbl.t;  (* var name -> number + type *)
  }

  let create ?(default=tptp_default) signature =
    let ctx = {
      default;
      signature;
      subst = S.empty;
      vars_ty = [];
      const_ty = [];
      locs = [];
      renaming = LogtkSubsts.Renaming.create ();
      symbols = LogtkSymbol.Tbl.create 7;
      tyvars = LogtkType.Conv.create ();
      vars = Hashtbl.create 7;
    } in
    ctx

  let copy t = { t with
    renaming = LogtkSubsts.Renaming.create ();
    symbols = LogtkSymbol.Tbl.copy t.symbols;
    tyvars = LogtkType.Conv.copy t.tyvars;
    vars = Hashtbl.copy t.vars;
  }

  let clear ctx =
    ctx.subst <- S.empty;
    ctx.const_ty <- [];
    ctx.vars_ty <- [];
    ctx.locs <- [];
    LogtkSymbol.Tbl.clear ctx.symbols;
    ctx.signature <- LogtkSignature.empty;
    LogtkType.Conv.clear ctx.tyvars;
    Hashtbl.clear ctx.vars;
    ()

  let exit_scope ctx =
    Hashtbl.clear ctx.vars;
    LogtkType.Conv.clear ctx.tyvars;
    LogtkSubsts.Renaming.clear ctx.renaming;
    ()

  let add_signature ctx signature =
    ctx.signature <- LogtkSignature.merge ctx.signature signature;
    ()

  let declare ctx sym ty =
    ctx.signature <- LogtkSignature.declare ctx.signature sym ty

  (* generate fresh type var. *)
  let _new_ty_var ctx = LogtkType.fresh_var ()

  (* generate [n] fresh type vars *)
  let rec _new_ty_vars ctx n =
    if n = 0
      then []
      else _new_ty_var ctx :: _new_ty_vars ctx (n-1)

  (* convert a prolog term into a type *)
  let ty_of_prolog ctx ty =
    LogtkType.Conv.of_prolog ~ctx:ctx.tyvars ty

  (* error-raising function *)
  let __error ctx msg =
    let b = Buffer.create 15 in
    (* print closest location *)
    begin match ctx.locs with
    | [] -> ()
    | loc::_ -> Printf.bprintf b "at %a: " Loc.pp loc
    end;
    Printf.kbprintf
      (fun b -> raise (LogtkType.Error (Buffer.contents b)))
      b msg

  (* obtain a (possibly fresh) type var for this name *)
  let _get_ty_var ctx name =
    match LogtkType.Conv.of_prolog ~ctx:ctx.tyvars name with
    | None -> assert false
    | Some v -> v

  (* variable number and type, for the given name. An optional type can
    be provided.*)
  let _get_var ?ty ctx name =
    try
      let n, ty' = Hashtbl.find ctx.vars name in
      match ty with
      | None -> n, ty'
      | Some ty ->
        (* check same type *)
        if LogtkType.eq ty ty'
          then n, ty'
          else failwith
            (LogtkUtil.sprintf "type mismatch for var %s: %a and %a"
              name LogtkType.pp ty LogtkType.pp ty')
    with Not_found ->
      let n = Hashtbl.length ctx.vars in
      let ty = match ty with
        | None ->
            (* for inferring polymorphic types! just be sure to bind it
             * somewhere. *)
            let ty = _new_ty_var ctx in
            ctx.vars_ty <- ty :: ctx.vars_ty;
            ty
        | Some ty -> ty
      in
      (*LogtkUtil.debug ~section 5 "var %s now has number %d and type %a" name n LogtkType.pp ty;*)
      Hashtbl.add ctx.vars name (n,ty);
      n, ty

  (* a rigid variable starts with "?" *)
  let _is_rigid_var_name name =
    name <> "" && name.[0] = '?'

  (* enter new scope for the variable with this name *)
  let _enter_var_scope ctx name ty =
    let n = Hashtbl.length ctx.vars in
    Hashtbl.add ctx.vars name (n,ty);
    n

  let _exit_var_scope ctx name =
    Hashtbl.remove ctx.vars name

  let with_loc ctx ~loc f =
    let old_locs = ctx.locs in
    ctx.locs <- loc::old_locs;
    LogtkUtil.finally
      ~h:(fun () -> ctx.locs <- old_locs)
      ~f

  (* unification of types, may raise LogtkUnif.Fail *)
  let __unif ctx ty1 ty2 =
    LogtkUnif.Ty.unification ~subst:ctx.subst ty1 0 ty2 0

  (* unify within the context's substitution. Wraps {!__unif}
     by returning a nicer exception in case of failure *)
  let unify ctx ty1 ty2 =
    try
      __unif ctx ty1 ty2
    with LogtkUnif.Fail ->
      let ty1 = LogtkSubsts.Ty.apply_no_renaming ctx.subst ty1 0 in
      let ty2 = LogtkSubsts.Ty.apply_no_renaming ctx.subst ty2 0 in
      __error ctx "could not unify types %a and %a" LogtkType.pp ty1 LogtkType.pp ty2

  (* same as {!unify}, but also updates the ctx's substitution *)
  let unify_and_set ctx ty1 ty2 =
    LogtkUtil.debug ~section 5 "unify types %a and %a" LogtkType.pp ty1 LogtkType.pp ty2;
    let subst = unify ctx ty1 ty2 in
    ctx.subst <- subst

  let constrain_type_type = unify_and_set

  (* Fresh function type with [arity] arguments *)
  let fresh_fun_ty ~arity ctx =
    let ret = _new_ty_var ctx in
    let new_vars = _new_ty_vars ctx arity in
    let ty = LogtkType.(ret <== new_vars) in
    (* only need to specialize the return type
       XXX justify/explain *)
    ctx.const_ty <- ret :: ctx.const_ty;
    ty

  (* If the function symbol has an unknown type, a fresh variable
     is returned. Otherwise the known type of the symbol is returned.
     @param arity the expected arity (if not declared) *)
  let type_of_fun ~arity ctx s =
    match s with
    | Sym.Int _ -> ctx.default.default_int
    | Sym.Rat _ -> ctx.default.default_rat
    | Sym.Conn Sym.Wildcard -> _new_ty_var ctx
    | Sym.Conn _
    | Sym.Cst _ ->
      begin match LogtkSignature.find ctx.signature s with
      | Some ty -> ty
      | None ->
        (* give a new type variable to this symbol. The symbol will not
          be able to be polymorphic (need to declare it!). *)
        try
          let ty = Sym.Tbl.find ctx.symbols s in
          ty
        with Not_found ->
          let ty = fresh_fun_ty ~arity ctx in
          Sym.Tbl.add ctx.symbols s ty;
          ty
      end

  let to_signature ctx =
    let signature = ctx.signature in
    (* enrich signature with new symbols *)
    Sym.Tbl.fold
      (fun s ty signature ->
        (* evaluate type. if variables remain, they are generalized *)
        let ty = S.Ty.apply_no_renaming ctx.subst ty 0 in
        (* generalize free vars, if any *)
        let ty = LogtkType.close_forall ty in
        (* add to signature *)
        LogtkSignature.declare signature s ty)
      ctx.symbols signature

  let __specialize_ty_var ctx v =
    try
      let subst = __unif ctx v ctx.default.default_i in
      ctx.subst <- subst
    with LogtkUnif.Fail -> ()

  let bind_to_default ctx =
    (* try to bind the variable. Will fail if already bound to
      something else, which is fine. *)
    List.iter (__specialize_ty_var ctx) ctx.const_ty;
    List.iter (__specialize_ty_var ctx) ctx.vars_ty;
    ctx.const_ty <- [];
    ctx.vars_ty <- [];
    ()

  let generalize ctx =
    (* keep constructor variables as they are, they will be generalized
        if {!to_signature} is called. *)
    List.iter (__specialize_ty_var ctx) ctx.const_ty;
    ctx.const_ty <- [];
    ctx.vars_ty <- [];
    ()

  let reset_renaming ctx =
    LogtkSubsts.Renaming.clear ctx.renaming

  (* evaluate the type in the current substitution *)
  let eval_ty ctx ty =
    LogtkSubsts.Ty.apply_no_renaming ctx.subst ty 0

  (* apply substitution to type *)
  let apply_ty ctx ty =
    LogtkSubsts.Ty.apply ~renaming:ctx.renaming ctx.subst ty 0

  (* apply substitution to term *)
  let apply_fo ctx t =
    LogtkSubsts.FO.apply ~renaming:ctx.renaming ctx.subst t 0

  (* apply substitution to ho term *)
  let apply_ho ctx t =
    LogtkSubsts.HO.apply ~renaming:ctx.renaming ctx.subst t 0
end

(** {2 Composition monad} *)

module MonadFun(Domain : sig type t end) = struct
  type domain = Domain.t
  type 'a fun_ = domain -> 'a
  type 'a t = 'a fun_
  type 'a monad = 'a fun_

  let return x _ = x

  let (>>=) f1 f2 x =
    f2 (f1 x) x

  let map f f1 x = f1 (f x)

  let fold (seq:'a Sequence.t) (acc:'b t) (f:'b -> 'a -> 'b t) =
    Sequence.fold
      (fun acc x ->
        fun elt -> (f (acc elt) x) elt)
      acc seq

  let fold_l l = fold (Sequence.of_list l)

  let map_l l f elt = List.map (fun x -> f x elt) l

  let seq l elt =
    List.map (fun f -> f elt) l
end

module Closure = MonadFun(Ctx)

(** {2 Hindley-Milner} *)

module type S = sig
  type untyped (** untyped term *)
  type typed   (** typed term *)

  val infer_exn : Ctx.t -> untyped -> LogtkType.t * typed Closure.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment!

        @param ctx the context
        @param untyped the untyped term whose type must be inferred

        @return the inferred type of the untyped term (possibly a type var)
          along with a closure to produce a typed term once every
          constraint has been solved
        @raise LogtkType.Error if the types are inconsistent *)

  val infer : Ctx.t -> untyped -> (LogtkType.t * typed Closure.t) or_error
    (** Safe version of {!infer_exn}. It returns [`Error s] rather
        than raising {!LogtkType.Error} if the typechecking fails. *)

  (** {3 Constraining types}

  This section is mostly useful for inferring a signature without
  converting untyped_terms into typed_terms. *)

  val constrain_term_term_exn : Ctx.t -> untyped -> untyped -> unit
    (** Force the two terms to have the same type in this context
        @raise LogtkType.Error if an inconsistency is detected *)

  val constrain_term_type_exn : Ctx.t -> untyped -> LogtkType.t -> unit
    (** Force the term's type and the given type to be the same.
        @raise LogtkType.Error if an inconsistency is detected *)

  val constrain_term_term : Ctx.t -> untyped -> untyped -> unit or_error
    (** Safe version of {!constrain_term_term_exn} *)

  val constrain_term_type : Ctx.t -> untyped -> LogtkType.t -> unit or_error
    (** Safe version of {!constrain_term_type_exn} *)
end

exception ExitSequence of string

let map_error_seq f seq =
  try
    let s = Sequence.map
      (fun x -> match f x with
        | `Error s -> raise (ExitSequence s)
        | `Ok y -> y
      ) seq
    in
    Err.return (Sequence.persistent s)
  with ExitSequence s ->
    Err.fail s

let _err_wrap1 f x =
  try Err.return (f x)
  with LogtkType.Error s -> Err.fail s

let _err_wrap2 f x y =
  try Err.return (f x y)
  with LogtkType.Error s -> Err.fail s

let _err_wrap3 f x y z =
  try Err.return (f x y z)
  with LogtkType.Error s -> Err.fail s

module FO = struct
  module PT = LogtkPrologTerm
  module T = LogtkFOTerm
  module F = LogtkFormula.FO

  type untyped = PT.t
  type typed = T.t
  type typed_form = F.t

  (* convert a list of terms into a list of types *)
  let rec _convert_type_args ctx l = match l with
  | [] -> []
  | t::l' ->
      begin match Ctx.ty_of_prolog ctx t with
      | None -> Ctx.__error ctx "term %a is not a type" PT.pp t
      | Some ty -> ty :: _convert_type_args ctx l'
      end

  (* infer a type for [t], possibly updating [ctx]. Also returns a
    continuation to build a typed term. *)
  let rec infer_rec ctx t =
    match t.PT.loc with
    | None -> infer_rec_view ctx t.PT.term
    | Some loc -> Ctx.with_loc ctx ~loc (fun () -> infer_rec_view ctx t.PT.term)
  and infer_rec_view ctx t = match t with
    | PT.Column ({PT.term=PT.Var name}, ty) ->
      (* typed var *)
      let ty = match Ctx.ty_of_prolog ctx ty with
        | Some ty -> ty
        | None -> Ctx.__error ctx "expected type, got %a" PT.pp ty
      in
      let i, ty = Ctx._get_var ctx ~ty name in
      LogtkUtil.debug ~section 5 "type of var %s: %a" name LogtkType.pp ty;
      ty, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i)
    | PT.Var name ->
      (* (possibly) untyped var *)
      let i, ty = Ctx._get_var ctx name in
      LogtkUtil.debug ~section 5 "type of var %s: %a" name LogtkType.pp ty;
      ty, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i)
    | PT.Const (Sym.Conn Sym.Wildcard) ->
      let ty = Ctx._new_ty_var ctx in
      (* generate fresh term variable *)
      let x = Sym.Base.fresh_var () in
      ty, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        Ctx.apply_fo ctx (T.const ~ty x))
    | PT.Const s ->
      let ty_s = Ctx.type_of_fun ~arity:0 ctx s in
      LogtkUtil.debug ~section 5 "type of symbol %a: %a" Sym.pp s LogtkType.pp ty_s;
      ty_s, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty_s in
        T.const ~ty s)
    | PT.Syntactic (s, l)
    | PT.App ({PT.term=PT.Const s}, l) ->
      (* use type of [s] *)
      let ty_s = Ctx.type_of_fun ~arity:(List.length l) ctx s in
      LogtkUtil.debug ~section 5 "type of symbol %a: %a" Sym.pp s LogtkType.pp ty_s;
      let n_tyargs, n_args = match LogtkType.arity ty_s with
        | LogtkType.NoArity -> 0, List.length l
        | LogtkType.Arity (a,b) -> a, b
      in
      (* separation between type arguments and proper term arguments,
          based on the expected arity of the symbol. The first
          [n_tyargs] arguments are converted to types, the remaining
          [n_args] ones are inferred as terms.
          XXX hack: special case for FO, if n_args=length l then type
          arguments are assumed to have been omitted*)
      let tyargs, args =
        if List.length l = n_args
          then Ctx._new_ty_vars ctx n_tyargs, l (* hack *)
          else
            let tyargs, args = LogtkUtil.list_split_at n_tyargs l in
            let tyargs = _convert_type_args ctx tyargs in
            tyargs, args
      in
      let ty_s' = LogtkType.apply_list ty_s tyargs in
      (* create sub-closures, by inferring the type of [args] *)
      let l = List.map (fun t' -> infer_rec ctx t') args in
      let ty_of_args, closure_args = List.split l in
      let closure_args = Closure.seq closure_args in
      (* [s] has type [ty_s] once applied to polymorphic type arguments,
          but must also have type [ty_l -> 'a].
          We generate a fresh variable 'a (named [ty_ret]),
          which is also the result. *)
      let ty_ret = Ctx._new_ty_var ctx in
      Ctx.unify_and_set ctx ty_s' (LogtkType.arrow_list ty_of_args ty_ret);
      (* now to produce the closure, that first creates subterms *)
      ty_ret, (fun ctx ->
        let args' = closure_args ctx in
        let tyargs' = List.map (Ctx.apply_ty ctx) tyargs in
        let ty_s' = LogtkType.close_forall (Ctx.apply_ty ctx ty_s) in
        T.app_full (T.const ~ty:ty_s' s) tyargs' args')
    | PT.Int n ->
        let ty = ctx.Ctx.default.default_int in
        ty, (fun _ -> T.const ~ty (LogtkSymbol.mk_int n))
    | PT.Rat n ->
        let ty = ctx.Ctx.default.default_rat in
        ty, (fun _ -> T.const ~ty (LogtkSymbol.mk_rat n))
    | PT.List _
    | PT.Column _
    | PT.App _
    | PT.Record _
    | PT.Bind _ -> Ctx.__error ctx "expected first-order term"

  let infer_var_scope ctx t = match t.PT.term with
    | PT.Column ({PT.term=PT.Var name}, ty) ->
      let ty = match Ctx.ty_of_prolog ctx ty with
        | Some ty -> ty
        | None -> Ctx.__error ctx "expected type, got %a" PT.pp ty
      in
      let i = Ctx._enter_var_scope ctx name ty in
      (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i)
    | PT.Var name ->
      let ty = ctx.Ctx.default.default_i in
      let i = Ctx._enter_var_scope ctx name ty in
      (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i)
    | _ -> assert false

  let exit_var_scope ctx t = match t.PT.term with
    | PT.Column ({PT.term=PT.Var name}, _)
    | PT.Var name -> Ctx._exit_var_scope ctx name
    | _ -> assert false

  let infer_exn ctx t =
    LogtkUtil.enter_prof prof_infer;
    LogtkUtil.debug ~section 5 "infer_term %a" PT.pp t;
    try
      let ty, k = infer_rec ctx t in
      LogtkUtil.exit_prof prof_infer;
      ty, k
    with (* error handling: return a nice message *)
    | e ->
      LogtkUtil.exit_prof prof_infer;
      raise e

  let infer ctx t = _err_wrap2 infer_exn ctx t

  let constrain_term_term_exn ctx t1 t2 =
    let ty1, _ = infer_exn ctx t1 in
    let ty2, _ = infer_exn ctx t2 in
    Ctx.unify_and_set ctx ty1 ty2

  let constrain_term_term ctx t1 t2 =
    _err_wrap3 constrain_term_term_exn ctx t1 t2

  let constrain_term_type_exn ctx t ty =
    let ty1, _ = infer_exn ctx t in
    Ctx.unify_and_set ctx ty1 ty

  let constrain_term_type ctx t ty =
    _err_wrap3 constrain_term_type_exn ctx t ty

  let rec infer_form_rec ctx f =
    match f.PT.loc with
    | None -> infer_form_rec_view ctx f
    | Some loc ->
      Ctx.with_loc ctx ~loc (fun () -> infer_form_rec_view ctx f)
  and infer_form_rec_view ctx f = match f.PT.term with
    | PT.Const (Sym.Conn ((Sym.True | Sym.False) as b)) ->
      fun _ ->
        begin match b with
        | Sym.True -> F.Base.true_
        | Sym.False -> F.Base.false_
        | _ -> assert false
        end
    | PT.Syntactic (Sym.Conn Sym.And, l) ->
      let l' = List.map (fun f' -> infer_form_rec ctx f') l in
      fun ctx ->
        let l' = (Closure.seq l') ctx in
        F.Base.and_ l'
    | PT.Syntactic (Sym.Conn Sym.Or, l) ->
      let l' = List.map (fun f' -> infer_form_rec ctx f') l in
      fun ctx ->
        let l' = (Closure.seq l') ctx in
        F.Base.or_ l'
    | PT.Syntactic (Sym.Conn ((Sym.Equiv | Sym.Xor | Sym.Imply) as conn), [a;b]) ->
      let a' = infer_form_rec ctx a  and b' = infer_form_rec ctx b in
      fun ctx ->
        let a = a' ctx and b = b' ctx in
        begin match conn with
        | Sym.Equiv -> F.Base.equiv a b
        | Sym.Xor -> F.Base.xor a b
        | Sym.Imply -> F.Base.imply a b
        | _ -> assert false
        end
    | PT.Syntactic (Sym.Conn Sym.Not, [a]) ->
      let a' = infer_form_rec ctx a in
      fun ctx -> F.Base.not_ (a' ctx)
    | PT.Bind(Sym.Conn ((Sym.Forall | Sym.Exists) as conn), vars, f') ->
      let vars' = Closure.seq (List.map (infer_var_scope ctx) vars) in
      let f' =
        LogtkUtil.finally
          ~h:(fun () -> List.iter (exit_var_scope ctx) (List.rev vars))
          ~f:(fun () -> infer_form_rec ctx f')
      in
      fun ctx ->
        begin match conn with
        | Sym.Forall -> F.Base.forall (vars' ctx) (f' ctx)
        | Sym.Exists -> F.Base.exists (vars' ctx) (f' ctx)
        | _ -> assert false
        end
    | PT.Bind(Sym.Conn Sym.ForallTy, vars, f') ->
      let vars' = List.map (Ctx._get_ty_var ctx) vars in
      if not (List.for_all LogtkType.is_var vars')
        then Ctx.__error ctx "expected type variables";
      let f' = infer_form_rec ctx f' in
      fun ctx ->
        let vars' = List.map (Ctx.apply_ty ctx) vars' in
        F.Base.forall_ty vars' (f' ctx)
    | PT.Syntactic (Sym.Conn ((Sym.Eq | Sym.Neq) as conn),
      ([_;a;b] | [_; {PT.term=PT.List [a;b]}] | [a;b])) ->
      (* a ?= b *)
      let tya, a = infer_exn ctx a in
      let tyb, b = infer_exn ctx b in
      Ctx.unify_and_set ctx tya tyb;
      fun ctx ->
        begin match conn with
        | Sym.Eq -> F.Base.eq (a ctx) (b ctx)
        | Sym.Neq -> F.Base.neq (a ctx) (b ctx)
        | _ -> assert false
        end
    | PT.Const _
    | PT.App _ ->
      (* atoms *)
      let tyt, t = infer_exn ctx f in
      Ctx.unify_and_set ctx tyt ctx.Ctx.default.default_prop;
      fun ctx -> F.Base.atom (t ctx)
    | PT.Var _
    | PT.Column _
    | PT.List _
    | PT.Int _
    | PT.Bind _
    | PT.Record _
    | PT.Syntactic _
    | PT.Rat _ -> Ctx.__error ctx "expected formula, got %a" PT.pp f

  let infer_form_exn ctx f =
    LogtkUtil.debug ~section 5 "infer_form %a" PT.pp f;
    try
      let c_f = infer_form_rec ctx f in
      c_f
    with e ->
      LogtkUtil.exit_prof prof_infer;
      raise e

  let infer_form ctx f = _err_wrap2 infer_form_exn ctx f

  let constrain_form_exn ctx f =
    let _ = infer_form ctx f in
    ()

  let constrain_form ctx f = _err_wrap2 constrain_form_exn ctx f

  let signature_forms_exn signature seq =
    let ctx = Ctx.create signature in
    Sequence.iter (constrain_form_exn ctx) seq;
    Ctx.to_signature ctx

  let signature_forms s seq = _err_wrap2 signature_forms_exn s seq

  let convert_exn ?(generalize=false) ~ctx t =
    let _, closure = infer_exn ctx t in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    closure ctx

  let convert ?generalize ~ctx t =
    _err_wrap1 (convert_exn ?generalize ~ctx) t

  let convert_form_exn ?(generalize=false) ~ctx f =
    let closure = infer_form_exn ctx f in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    closure ctx

  let convert_form ?generalize ~ctx f =
    _err_wrap1 (convert_form_exn ?generalize ~ctx) f

  let convert_clause_exn ?(generalize=false) ~ctx c =
    let closures = List.map (fun lit -> infer_form_exn ctx lit) c in
    Ctx.exit_scope ctx;
    (* use same renaming for all formulas, to keep
      a consistent scope *)
    Ctx.reset_renaming ctx;
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    List.map (fun c' -> c' ctx) closures

  let convert_clause ?generalize ~ctx f =
    _err_wrap1 (convert_clause_exn ?generalize ~ctx) f

  let convert_seq_exn ?(generalize=false) input forms =
    let ctx = match input with
      | `ctx c -> c
      | `sign s -> Ctx.create s
    in
    (* build closures, inferring all types *)
    let closures = Sequence.map (fun f -> infer_form_exn ctx f) forms in
    let closures = Sequence.to_rev_list closures in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    (* apply closures to the final substitution *)
    List.rev_map (fun c -> c ctx) closures

  let convert_seq ?generalize i forms =
    _err_wrap2 (convert_seq_exn ?generalize) i forms
end

module HO = struct
  module PT = LogtkPrologTerm
  module T = LogtkHOTerm

  type untyped = PT.t
  type typed = T.t

  (* convert a list of terms into a list of types *)
  let rec _convert_type_args ctx l = match l with
  | [] -> []
  | t::l' ->
      begin match Ctx.ty_of_prolog ctx t with
      | None -> Ctx.__error ctx "term %a is not a type" PT.pp t
      | Some ty -> ty :: _convert_type_args ctx l'
      end

  let infer_var_scope ctx t = match t.PT.term with
    | PT.Column ({PT.term=PT.Var name}, ty) ->
      let ty = match Ctx.ty_of_prolog ctx ty with
        | Some ty -> ty
        | None -> Ctx.__error ctx "expected type, got %a" PT.pp ty
      in
      let i = Ctx._enter_var_scope ctx name ty in
      ty, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i)
    | PT.Var name ->
      let ty = ctx.Ctx.default.default_i in
      let i = Ctx._enter_var_scope ctx name ty in
      ty, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i)
    | _ -> assert false

  let exit_var_scope ctx t = match t.PT.term with
    | PT.Column ({PT.term=PT.Var name}, _)
    | PT.Var name -> Ctx._exit_var_scope ctx name
    | _ -> assert false

  (* infer a type for [t], possibly updating [ctx]. Also returns a
      continuation to build a typed term
      @param arity expected number of arguments *)
  let rec infer_rec ?(arity=0) ctx t =
    match t.PT.loc with
    | None -> infer_rec_view ~arity ctx t.PT.term
    | Some loc -> Ctx.with_loc ctx ~loc (fun () -> infer_rec_view ~arity ctx t.PT.term)
  and infer_rec_view ~arity ctx t = match t with
    | PT.Column ({PT.term=PT.Var name}, ty) ->
      (* typed var *)
      let ty = match Ctx.ty_of_prolog ctx ty with
        | Some ty -> ty
        | None -> Ctx.__error ctx "expected type, got %a" PT.pp ty
      in
      let i, ty = Ctx._get_var ctx ~ty name in
      ty, (fun ctx ->
        let ty' = Ctx.apply_ty ctx ty in
        if Ctx._is_rigid_var_name name
          then T.rigid_var ~ty:ty' i
          else T.var ~ty:ty' i)
    | PT.Var name ->
      (* (possibly) untyped var *)
      let i, ty = Ctx._get_var ctx name in
      ty, (fun ctx ->
        let ty' = Ctx.apply_ty ctx ty in
        if Ctx._is_rigid_var_name name
          then T.rigid_var ~ty:ty' i
          else T.var ~ty:ty' i)
    | PT.Const (Sym.Conn Sym.Wildcard) ->
      let ty = Ctx._new_ty_var ctx in
      (* generate fresh term variable *)
      let x = Sym.Base.fresh_var () in
      ty, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        Ctx.apply_ho ctx (T.const ~ty x))
    | PT.Bind (Sym.Conn Sym.Lambda, [], t) ->
      infer_rec ~arity ctx t
    | PT.Bind (Sym.Conn Sym.Lambda, [v], t) ->
      let ty_v, clos_v = infer_var_scope ctx v in
      let ty_t, clos_t = LogtkUtil.finally
        ~f:(fun () -> infer_rec ctx t)
        ~h:(fun () -> exit_var_scope ctx v)
      in
      (* type is ty_v -> ty_t *)
      let ty = LogtkType.(ty_t <=. ty_v) in
      ty, (fun ctx ->
        let t' = clos_t ctx in
        let v' = clos_v ctx in
        T.mk_lambda [v'] t')
    | PT.Bind (Sym.Conn Sym.Lambda, v::vs, t) ->
      (* on-the-fly conversion to unary lambdas *)
      infer_rec ~arity ctx (PT.TPTP.lambda [v] (PT.TPTP.lambda vs t))
    | PT.List l ->
      let ty_l, l' = List.split (List.map (infer_rec ~arity ctx) l) in
      let l' = Closure.seq l' in
      (* what is the type of the elements of the multiset? *)
      let ty_arg = match ty_l with
        | [] -> Ctx._new_ty_var ctx  (* empty multiset, any type! *)
        | ty::ty_l' ->
          (* make sure all elements have the same type *)
          List.iter (Ctx.unify_and_set ctx ty) ty_l';
          ty
      in
      (* the return type is multiset(ty_arg) *)
      let ty = LogtkType.multiset ty_arg in
      ty, (fun ctx ->
        let ty_arg = Ctx.apply_ty ctx ty_arg and l' = l' ctx in
        T.multiset ~ty:ty_arg l')
    | PT.Record (l, rest) ->
      let ty_l, l' = List.split
        (List.map
          (fun (n,t) ->
            let ty_t, t' = infer_rec ctx t in
            (n,ty_t), (fun ctx -> n, t' ctx))
          l)
      in
      let l' = Closure.seq l' in
      let ty_rest, rest' = match rest with
        | None -> None, (fun ctx -> None)
        | Some r ->
            let ty_r, r' = infer_rec ctx r in
            (* force [ty_r] to be a record *)
            let ty_record = LogtkType.record [] ~rest:(Some (Ctx._new_ty_var ctx)) in
            Ctx.unify_and_set ctx ty_r ty_record;
            Some ty_r, (fun ctx -> Some (r' ctx))
      in
      let ty = LogtkType.record ty_l ~rest:ty_rest in
      ty, (fun ctx ->
        T.record (l' ctx) ~rest:(rest' ctx))
    | PT.Const s ->
      (* recover the type *)
      let ty = Ctx.type_of_fun ~arity ctx s in
      ty, (fun ctx ->
        let ty = Ctx.apply_ty ctx ty in
        T.const ~ty s)
    | PT.Syntactic (s, l) ->
      (* reduce to next case *)
      infer_rec ~arity ctx (PT.app (PT.const s) l)
    | PT.App (t, []) -> infer_rec ~arity ctx t
    | PT.App (t, l) ->
      (* we are going to assume that the type of [t], as inferred, is a forall
          or a function (or a constant iff [l] is empty.
          We then evaluate the term to be sure to get its most precise
          definition so far (e.g. if F = (f a), the type of F may be
          more precise than just a type var. *)
      let ty_t, clos_t = infer_rec ~arity:(List.length l) ctx t in
      let n_tyargs, n_args = match LogtkType.arity (Ctx.eval_ty ctx ty_t) with
        | LogtkType.NoArity -> 0, List.length l
        | LogtkType.Arity (a,b) -> a, b
      in
      (*LogtkUtil.debug ~section 5 "fun %a : %a expects %d type args and %d args (applied to %a)"
        PT.pp t LogtkType.pp ty_t n_tyargs n_args (LogtkUtil.pp_list PT.pp) l; *)
      (* separation between type arguments and proper term arguments,
          based on the expected arity of the head [t].
          We split [l] into the list [tyargs], containing [n_tyargs] types,
          and [args], containing [n_args] terms. *)
      let tyargs, args = LogtkUtil.list_split_at n_tyargs l in
      let tyargs = _convert_type_args ctx tyargs in
      let ty_t' = LogtkType.apply_list ty_t tyargs in
      (* create sub-closures, by inferring the type of [args] *)
      let l = List.map (infer_rec ctx) args in
      let ty_of_args, closure_args = List.split l in
      let closure_args = Closure.seq closure_args in
      (* [s] has type [ty_s] once applied to polymorphic type arguments,
          but must also have type [ty_l -> 'a].
          We generate a fresh variable 'a (named [ty_ret]),
          which is also the result. *)
      let ty_ret = Ctx._new_ty_var ctx in
      Ctx.unify_and_set ctx ty_t' (LogtkType.arrow_list ty_of_args ty_ret);
      (* closure *)
      ty_ret, (fun ctx ->
        let args' = closure_args ctx in
        let tyargs' = List.map (Ctx.apply_ty ctx) tyargs in
        let t' = clos_t ctx in
        T.at_full t' ~tyargs:tyargs' args')
    | PT.Int n ->
        let ty = ctx.Ctx.default.default_int in
        ty, (fun _ -> T.const ~ty (LogtkSymbol.mk_int n))
    | PT.Rat n ->
        let ty = ctx.Ctx.default.default_rat in
        ty, (fun _ -> T.const ~ty (LogtkSymbol.mk_rat n))
    | PT.Column _
    | PT.Bind _ ->
        Ctx.__error ctx "expected higher-order term"

  let infer_exn ctx t =
    LogtkUtil.enter_prof prof_infer;
    LogtkUtil.debug ~section 5 "infer_term %a" PT.pp t;
    try
      let ty, k = infer_rec ctx t in
      LogtkUtil.exit_prof prof_infer;
      ty, k
    with
    | e ->
      LogtkUtil.exit_prof prof_infer;
      raise e

  let infer ctx t = _err_wrap2 infer_exn ctx t

  let constrain_term_term_exn ctx t1 t2 =
    let ty1, _ = infer_exn ctx t1 in
    let ty2, _ = infer_exn ctx t2 in
    Ctx.unify_and_set ctx ty1 ty2

  let constrain_term_term ctx t1 t2 =
    _err_wrap3 constrain_term_term_exn ctx t1 t2

  let constrain_term_type_exn ctx t ty =
    let ty1, _ = infer_exn ctx t in
    Ctx.unify_and_set ctx ty1 ty

  let constrain_term_type ctx t ty =
    _err_wrap3 constrain_term_type_exn ctx t ty

  let constrain_exn ~ctx t =
    let ty, _ = infer_exn ctx t in
    Ctx.unify_and_set ctx ty LogtkType.TPTP.o;
    ()

  let constrain ~ctx t = _err_wrap1 (constrain_exn ~ctx) t

  let convert_exn ?(generalize=false) ?(ret=LogtkType.TPTP.o) ~ctx t =
    let ty, closure = infer_exn ctx t in
    Ctx.unify_and_set ctx ty ret;
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    closure ctx

  let convert ?generalize ?ret ~ctx t =
    _err_wrap1 (convert_exn ?generalize ?ret ~ctx) t

  let convert_seq_exn ?(generalize=false) ~ctx terms =
    let closures = Sequence.map
      (fun t ->
        let ty, closure = infer_exn ctx t in
        Ctx.unify_and_set ctx ty LogtkType.TPTP.o;
        closure)
      terms
    in
    (* evaluate *)
    let closures = Sequence.to_rev_list closures in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    List.rev_map
      (fun c -> c ctx)
      closures

  let convert_seq ?generalize ~ctx terms =
    _err_wrap1 (convert_seq_exn ?generalize ~ctx) terms
end
