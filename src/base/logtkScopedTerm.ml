
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Scoped Terms} *)

module Hash = CCHash
module Sym = LogtkSymbol

type symbol = Sym.t

module Kind = struct
  type t =
    | Kind
    | Type
    | FOTerm
    | HOTerm
    | Formula of t
    | Untyped
    | Generic  (* other terms *)
end

(* term *)
type t = {
  term : view;
  ty : type_result;
  kind : Kind.t;
  mutable id : int;
  mutable flags : int;
}
(* head form *)
and view =
  | Var of int              (** Free variable *)
  | RigidVar of int         (** Variable that only unifies with other rigid variables *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | Bind of symbol * t * t  (** LogtkType, sub-term *)
  | Const of symbol         (** Constant *)
  | Record of (string * t) list * t option (** Extensible record *)
  | RecordGet of t * string       (** [get r name] is [r.name] *)
  | RecordSet of t * string * t   (** [set r name t] is [r.name <- t] *)
  | Multiset of t list      (** Multiset of terms *)
  | App of t * t list       (** Uncurried application *)
  | At of t * t             (** Curried application *)
  | SimpleApp of symbol * t list  (** For representing special constructors *)
and type_result =
  | NoType
  | HasType of t

type term = t

let view t = t.term
let ty t = t.ty
let ty_exn t = match t.ty with
  | NoType -> raise (Invalid_argument "LogtkScopedTerm.ty_exn")
  | HasType ty -> ty
let kind t = t.kind

let hash_fun t s = Hash.int_ t.id s
let hash t = Hash.apply hash_fun t
let eq t1 t2 = t1 == t2
let cmp t1 t2 = Pervasives.compare t1.id t2.id

let _hash_ty t h =
  match t.ty with
  | NoType -> h
  | HasType ty -> Hash.int_ ty.id (Hash.string_ "type" h)

let _hash_norec t h =
  let h = match view t with
  | Var i -> h |> Hash.string_ "var" |> Hash.int_ i
  | RigidVar i -> h |> Hash.string_ "rigid" |> Hash.int_ i
  | BVar i -> h |> Hash.string_ "bvar" |> Hash.int_ i
  | Bind (s, varty, t') ->
      h |> Hash.string_ "bind" |> Sym.hash_fun s |> hash_fun varty |> hash_fun t'
  | Const s -> h |> Hash.string_ "const" |> Sym.hash_fun s
  | Record (l, rest) ->
      h
      |> Hash.string_ "record"
      |> Hash.list_ (fun (s,t') h -> h |> Hash.string_ s |> hash_fun t') l
      |> Hash.opt hash_fun rest
  | RecordGet (t, name) ->
      h |> Hash.string_ "get" |> hash_fun t |> Hash.string_ name
  | RecordSet (t, name, t') ->
      h |> Hash.string_ "set" |> hash_fun t |> Hash.string_ name |> hash_fun t'
  | Multiset l -> h |> Hash.string_ "ms" |> Hash.list_ hash_fun l
  | App (f, l) -> h |> Hash.string_ "app" |> hash_fun f |> Hash.list_ hash_fun l
  | At (t1, t2) -> h |> Hash.string_ "at" |> hash_fun t1 |> hash_fun t2
  | SimpleApp (s, l) ->
      h |> Hash.string_ "sapp" |> Sym.hash_fun s |> Hash.list_ hash_fun l
  in
  _hash_ty t h

let rec _eq_norec t1 t2 =
  t1.kind = t2.kind &&
  _eq_ty t1 t2 &&
  match t1.term, t2.term with
  | Var i, Var j
  | RigidVar i, RigidVar j -> i = j
  | BVar i, BVar j -> i = j
  | Const s1, Const s2 -> Sym.eq s1 s2
  | Bind (s1, varty1, t1'), Bind (s2, varty2, t2') ->
    Sym.eq s1 s2 && eq varty1 varty2 && eq t1' t2'
  | App (f1, l1), App (f2, l2) ->
    eq f1 f2 && _eq_list l1 l2
  | Multiset l1, Multiset l2 ->
    _eq_list l1 l2
  | Record (l1, None), Record (l2, None) ->
    _eq_record_list l1 l2
  | Record (l1, Some r1), Record (l2, Some r2) ->
    eq r1 r2 && _eq_record_list l1 l2
  | At (l1, r1), At (l2, r2) -> eq l1 l2 && eq r1 r2
  | SimpleApp (s1, l1), SimpleApp (s2, l2) ->
    Sym.eq s1 s2 && _eq_list l1 l2
  | _ -> false
and _eq_ty t1 t2 = match t1.ty, t2.ty with
  | NoType, NoType -> true
  | HasType ty1, HasType ty2 -> eq ty1 ty2
  | _ -> false
and _eq_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | t1::l1', t2::l2' -> eq t1 t2 && _eq_list l1' l2'
and _eq_record_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | (n1,t1)::l1', (n2,t2)::l2' -> n1=n2 && eq t1 t2 && _eq_record_list l1' l2'

(** {3 Flags} *)

type flag=int
let _flag_gen = LogtkUtil.Flag.create ()
let new_flag () = LogtkUtil.Flag.get_new _flag_gen
let set_flag t flag = t.flags <- t.flags lor flag
(*let unset_flag t flag = t.flags <- t.flags land (lnot flag)*)
let get_flag t flag = (t.flags land flag) != 0

(* groundness *)
let flag_ground = new_flag()

let ground t = get_flag t flag_ground

(* DB-closedness. We use two flags because this computation is lazy. *)
let flag_db_closed_computed = new_flag()
let flag_db_closed = new_flag()

(** {3 Constructors} *)

module H = LogtkHashcons.Make(struct
  type t = term
  let equal = _eq_norec
  let hash = Hash.apply _hash_norec
  let tag i t = assert (t.id = ~-1); t.id <- i
end)

let hashcons_stats () = H.stats ()

exception IllFormedTerm of string
type nat = int

let _make ~kind ~ty term = {
  term;
  kind;
  ty;
  id = ~-1;
  flags=0;
}

let _make_id ~id ~kind ~ty term = {
  term;
  kind;
  ty;
  id;
  flags=0;
}

let const ~kind ~ty s =
  let my_t = _make ~kind ~ty:(HasType ty) (Const s) in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty then set_flag t flag_ground;
  end;
  t

let app ~kind ~ty f l =
  match l with
  | [] -> f
  | _::_ ->
      let my_t = _make ~kind ~ty:(HasType ty) (App (f,l)) in
      let t = H.hashcons my_t in
      if t == my_t then begin
        if ground ty && ground f && List.for_all ground l
          then set_flag t flag_ground;
      end;
      t

let _var ~kind ~ty i =
  H.hashcons (_make ~kind ~ty:(HasType ty) (Var i))

let var ~kind ~ty i =
  if i<0 then raise (IllFormedTerm "var");
  _var ~kind ~ty i

let fresh_var =
  let r = ref ~-1 in
  fun ~kind ~ty () ->
    if !r >= 0 then failwith "LogtkScopedTerm.fresh_var: overflow";
    let t = _var ~kind ~ty !r in
    decr r;
    t

let bvar ~kind ~ty i =
  if i<0 then raise (IllFormedTerm "bvar");
  H.hashcons (_make ~kind ~ty:(HasType ty) (BVar i))

let rigid_var ~kind ~ty i =
  if i<0 then raise (IllFormedTerm "rigid_var");
  H.hashcons (_make ~kind ~ty:(HasType ty) (RigidVar i))

let bind ~kind ~ty ~varty s t' =
  H.hashcons (_make ~kind ~ty:(HasType ty) (Bind (s, varty, t')))

(* merge l1 and l2, which are both sorted. If the same key occurs with
 * distinct values, fail. *)
let rec __merge_records l1 l2 = match l1, l2 with
  | [], [] -> []
  | [], _ -> l2
  | _, [] -> l1
  | (n1,t1)::l1', (n2,t2)::l2' ->
      match String.compare n1 n2 with
      | 0 ->
        if eq t1 t2
          then (n1,t1) :: __merge_records l1' l2'  (* compatible *)
          else failwith ("ill-formed record: field "^n1^" has distinct values")
      | n when n < 0 -> (n1,t1) :: __merge_records l1' l2
      | _ -> (n2,t2) :: __merge_records l1 l2'

(* if any string of [l] appears in [seen], fail *)
let rec __check_duplicates seen l = match l with
  | [] -> ()
  | (s,_) :: l' ->
      if List.mem s seen
      then failwith ("ill-formed record: field " ^ s ^ " appears twice")
      else __check_duplicates (s::seen) l'

(* actually build the record *)
let __make_record ~kind ~ty l ~rest =
  let my_t = _make ~kind ~ty:(HasType ty) (Record (l,rest)) in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty && List.for_all (fun (_,t) -> ground t) l
    && (match rest with None -> true | Some r -> ground r)
      then set_flag t flag_ground;
  end;
  t

(* flatten record: if the rest is a record, merge its fields into [l].
 * precondition: [l] is sorted. *)
let rec __flatten_record ~kind ~ty l ~rest =
  match rest with
  | None -> __make_record ~kind ~ty l ~rest
  | Some r ->
      begin match view r with
      | Record (l', rest') ->
          (* here be flattening! *)
          __flatten_record ~kind ~ty (__merge_records l l') ~rest:rest'
      | _ -> __make_record ~kind ~ty l ~rest
      end

let record ~kind ~ty l ~rest =
  let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
  (* check that [l] is well-formed *)
  __check_duplicates [] l;
  __flatten_record ~kind ~ty l ~rest

let record_get ~kind ~ty r name =
  let my_t = _make ~kind ~ty:(HasType ty) (RecordGet(r,name)) in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty && ground r
      then set_flag t flag_ground;
  end;
  t

let record_set ~kind ~ty r name sub =
  let my_t = _make ~kind ~ty:(HasType ty) (RecordSet(r,name,sub)) in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty && ground r && ground sub
      then set_flag t flag_ground;
  end;
  t

let multiset ~kind ~ty l =
  let l = List.sort cmp l in
  let my_t = _make ~kind ~ty:(HasType ty) (Multiset l) in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty && List.for_all ground l
      then set_flag t flag_ground;
  end;
  t

let at ~kind ~ty l r =
  let my_t = _make ~kind ~ty:(HasType ty) (At (l,r)) in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty && ground l && ground r
      then set_flag t flag_ground;
  end;
  t

let simple_app ~kind ~ty s l =
  let my_t = _make ~kind ~ty:(HasType ty) (SimpleApp (s,l)) in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty && List.for_all ground l
      then set_flag t flag_ground;
  end;
  t

let mk_at = at

let tType =
  let my_t = _make ~kind:Kind.Kind ~ty:NoType (Const (Sym.Conn Sym.TType)) in
  set_flag my_t flag_ground;
  H.hashcons my_t

let cast ~ty old = match old.term with
  | Var i -> _var ~kind:old.kind ~ty i
  | RigidVar i -> rigid_var ~kind:old.kind ~ty i
  | BVar i -> bvar ~kind:old.kind ~ty i
  | Const s -> const ~kind:old.kind ~ty s
  | Bind (s, varty, t') -> bind ~kind:old.kind ~ty s ~varty t'
  | Record (l, rest) -> record ~kind:old.kind ~ty l ~rest
  | RecordGet (r, name) -> record_get ~kind:old.kind ~ty r name
  | RecordSet (r, name, sub) -> record_set ~kind:old.kind ~ty r name sub
  | Multiset l -> multiset ~kind:old.kind ~ty l
  | App (f,l) -> app ~kind:old.kind ~ty f l
  | At (f,a) -> at ~kind:old.kind ~ty f a
  | SimpleApp (s,l) -> simple_app ~kind:old.kind ~ty s l

let change_kind ~kind old =
  let my_t = { old with kind; id= ~-1; } in
  H.hashcons my_t

let is_var t = match view t with | Var _ -> true | _ -> false
let is_bvar t = match view t with | BVar _ -> true | _ -> false
let is_rigid_var t = match view t with | RigidVar _ -> true | _ -> false
let is_const t = match view t with | Const _ -> true | _ -> false
let is_bind t = match view t with | Bind _ -> true | _ -> false
let is_record t = match view t with | Record _ -> true | _ -> false
let is_record_get t = match view t with | RecordGet _ -> true | _ -> false
let is_record_set t = match view t with | RecordSet _ -> true | _ -> false
let is_multiset t = match view t with | Multiset _ -> true | _ -> false
let is_app t = match view t with | App _ -> true | _ -> false
let is_at t = match view t with | At _ -> true | _ -> false

(** {3 Containers} *)

module T = struct
  type t = term
  let equal = eq
  let hash = hash
  let compare = cmp
end

module Set = Sequence.Set.Make(T)
module Map = Sequence.Map.Make(T)

module Tbl = struct
  include Hashtbl.Make(T)

  let of_seq ?(init=create 7) seq =
    seq (fun (k,v) -> replace init k v);
    init

  let to_seq tbl k =
    iter (fun key v -> k (key,v)) tbl

  let to_list tbl =
    fold (fun k v acc -> (k,v)::acc) tbl []

  let of_list ?init l = of_seq ?init (Sequence.of_list l)
end

(** {3 De Bruijn} *)

module DB = struct
  type env = t LogtkDBEnv.t

  (* sequence2 of [De Bruijn, depth] pairs *)
  let rec _to_seq ~depth t k =
    begin match t.ty with
      | NoType -> ()
      | HasType ty -> _to_seq ~depth ty k
    end;
    match view t with
    | BVar _ -> k t depth
    | Var _
    | RigidVar _
    | Const _ -> ()
    | Bind (_, varty, t') ->
        _to_seq ~depth varty k;
        _to_seq ~depth:(depth+1) t' k
    | Record (l, rest) ->
        begin match rest with
          | None -> ()
          | Some r -> _to_seq~depth r k
        end;
        List.iter (fun (_,t) -> _to_seq ~depth t k) l
    | RecordGet (r, name) -> _to_seq ~depth r k
    | RecordSet (r, name, sub) -> _to_seq ~depth r k; _to_seq ~depth sub k
    | SimpleApp (_, l)
    | Multiset l ->
        List.iter (fun t -> _to_seq ~depth t k) l
    | App (f, l) ->
        _to_seq ~depth f k;
        List.iter (fun t -> _to_seq ~depth t k) l
    | At (l,r) ->
        _to_seq ~depth l k;
        _to_seq ~depth r k

  let _id x = x

  let _closed t =
    _to_seq ~depth:0 t
      |> Sequence.map2
        (fun bvar depth -> match view bvar with
          | BVar i -> i<depth
          | _ -> assert false
        )
      |> Sequence.for_all _id

  let closed t =
    (* depth=0, see whether result is cached *)
    if get_flag t flag_db_closed_computed
      then get_flag t flag_db_closed
      else begin
        (* compute and cache result *)
        let res = _closed t in
        set_flag t flag_db_closed_computed;
        if res then set_flag t flag_db_closed;
        res
      end

  (* check whether t contains the De Bruijn symbol n *)
  let contains t n =
    _to_seq ~depth:0 t
      |> Sequence.map2
        (fun bvar depth -> match view bvar with
          | BVar i -> i=n
          | _ -> assert false
        )
      |> Sequence.exists _id

  (* maps the term to another term, calling [on_binder acc t]
    when it meets a binder, and [on_bvar acc t] when it meets a
    bound variable. *)
  let _fold_map ~depth acc ~on_bvar ~on_binder t =
    let rec recurse ~depth acc t = match t.ty with
    | NoType ->
      assert (t == tType);
      t
    | HasType ty ->
      let ty = recurse ~depth acc ty in
      match view t with
        | Var i -> _var ~kind:t.kind ~ty i
        | RigidVar i -> rigid_var ~kind:t.kind ~ty i
        | Const s -> const ~kind:t.kind ~ty s
        | BVar i ->
          on_bvar ~kind:t.kind ~ty ~depth acc i
        | Bind (s, varty, t') ->
          let acc' = on_binder ~kind:t.kind ~ty ~depth acc s varty in
          let varty' = recurse ~depth acc varty in
          let t' = recurse ~depth:(depth+1) acc' t' in
          bind ~kind:t.kind ~ty ~varty:varty' s t'
        | Record (l, rest) ->
          let rest = match rest with
            | None -> None
            | Some r -> Some (recurse ~depth acc r)
          in
          let l = List.map (fun (s,t') -> s, recurse ~depth acc t') l in
          record ~kind:t.kind ~ty l ~rest
        | RecordGet (r,name) ->
          record_get ~kind:t.kind ~ty (recurse ~depth acc r) name
        | RecordSet (r,name,sub) ->
          let r = recurse ~depth acc r in
          let sub = recurse ~depth acc sub in
          record_set ~kind:t.kind ~ty r name sub
        | Multiset l ->
          let l = List.map (recurse ~depth acc) l in
          multiset ~kind:t.kind ~ty l
        | App (f, l) ->
          app ~kind:t.kind ~ty (recurse ~depth acc f) (List.map (recurse ~depth acc) l)
        | At (l,r) ->
          at ~kind:t.kind ~ty (recurse ~depth acc l) (recurse ~depth acc r)
        | SimpleApp (s,l) ->
          simple_app ~kind:t.kind ~ty s (List.map (recurse ~depth acc) l)
    in
    recurse ~depth:0 acc t

  (* shift the non-captured De Bruijn indexes in the term by n *)
  let shift n t =
    assert (n >= 0);
    _fold_map ~depth:0 ()
      ~on_bvar:(
        fun ~kind ~ty ~depth () i ->
          if i >= depth
            then (* shift *)
              bvar ~kind ~ty (i + n)
            else bvar ~kind ~ty i
        )
      ~on_binder:(fun ~kind ~ty ~depth () _ _ -> ())
      t

  let unshift n t =
    _fold_map ~depth:0 ()
      ~on_bvar:(
        fun ~kind ~ty ~depth () i ->
          if i >= depth
            then (* ushift *)
              bvar ~kind ~ty (i - n)
            else bvar ~kind ~ty i
        )
      ~on_binder:(fun ~kind ~ty ~depth () _ _ -> ())
      t

  let open_vars t =
    _to_seq ~depth:0 t
      |> Sequence.zip
      |> Sequence.filter_map
        (fun (t,depth) -> match view t with
          | BVar i when i>=depth ->
              let ty = unshift depth (ty_exn t) in
              Some (bvar ~kind:t.kind ~ty (i-depth))
          | _ -> None
        )

  (* recurse and replace [sub]. *)
  let rec _replace depth ~sub t =
    match t.ty with
    | NoType ->
      assert (t == tType);
      t
    | HasType ty ->
      let ty = _replace depth ty ~sub in
      match view t with
        | _ when eq t sub ->
          bvar ~kind:t.kind ~ty depth  (* replace *)
        | Var i -> _var ~kind:t.kind ~ty i
        | RigidVar i -> rigid_var ~kind:t.kind ~ty i
        | BVar i -> bvar ~kind:t.kind ~ty i
        | Const s -> const ~kind:t.kind ~ty s
        | Bind (s, varty, t') ->
          let varty' = _replace depth ~sub varty in
          let t' = _replace (depth+1) t' ~sub in
          bind ~kind:t.kind ~ty ~varty:varty' s t'
        | Record (l, rest) ->
          let rest = CCOpt.map (_replace depth ~sub) rest in
          let l = List.map (fun (s,t') -> s, _replace depth t' ~sub) l in
          record ~kind:t.kind ~ty l ~rest
        | RecordGet (r, name) ->
          let r = _replace depth ~sub r in
          record_get ~kind:t.kind ~ty r name
        | RecordSet (r, name, rhs) ->
          let r = _replace depth ~sub r in
          let rhs = _replace depth ~sub rhs in
          record_set ~kind:t.kind ~ty r name rhs
        | Multiset l ->
          let l = List.map (_replace depth ~sub) l in
          multiset ~kind:t.kind ~ty l
        | App (f, l) ->
          app ~kind:t.kind ~ty (_replace depth ~sub f) (List.map (_replace depth ~sub) l)
        | At (l,r) ->
          at ~kind:t.kind ~ty (_replace depth l ~sub) (_replace depth r ~sub)
        | SimpleApp (s,l) ->
          simple_app ~kind:t.kind ~ty s (List.map (_replace depth ~sub) l)

  let replace t ~sub = _replace 0 t ~sub

  let from_var t ~var =
    assert (is_var var);
    replace t ~sub:var

  let rec _eval env t =
    match t.ty with
    | NoType ->
      assert (t == tType);
      t
    | HasType ty ->
      let ty = _eval env ty in
      match view t with
        | _ when ground t -> t
        | Var i -> _var ~kind:t.kind ~ty i
        | RigidVar i -> rigid_var ~kind:t.kind ~ty i
        | Const s -> const ~kind:t.kind ~ty s
        | BVar i ->
          begin match LogtkDBEnv.find env i with
            | None -> bvar ~kind:t.kind ~ty i
            | Some t' ->
              assert (closed t');
              t'
          end
        | Bind (s, varty, t') ->
          let varty' = _eval env varty in
          let t' = _eval (LogtkDBEnv.push_none env) t' in
          bind ~kind:t.kind ~ty ~varty:varty' s t'
        | Record (l, rest) ->
          let rest = CCOpt.map (_eval env) rest in
          let l = List.map (fun (s,t') -> s, _eval env t') l in
          record ~kind:t.kind ~ty l ~rest
        | RecordGet(r, name) ->
          record_get ~kind:t.kind ~ty (_eval env r) name
        | RecordSet(r, name, sub) ->
          record_set ~kind:t.kind ~ty (_eval env r) name (_eval env sub)
        | Multiset l ->
          let l = List.map (_eval env) l in
          multiset ~kind:t.kind ~ty l
        | App (f, l) ->
          app ~kind:t.kind ~ty (_eval env f) (List.map (_eval env) l)
        | At (l,r) ->
          at ~kind:t.kind ~ty (_eval env l) (_eval env r)
        | SimpleApp (s,l) ->
          simple_app ~kind:t.kind ~ty s (List.map (_eval env) l)

  let eval env t = _eval env t
end

let bind_vars ~kind ~ty s vars t =
  List.fold_right
    (fun v t ->
      assert (is_var v);
      let t' = DB.replace (DB.shift 1 t) ~sub:v in
      let varty = match v.ty with
        | NoType -> failwith "LogtkScopedTerm.bind_vars: variable has no type"
        | HasType ty -> ty
      in
      bind ~kind ~ty ~varty s t')
    vars t

(** {3 Iterators} *)

module Seq = struct
  let vars t k =
    let rec vars t = match view t with
      | _ when ground t -> ()
      | Var _ -> k t
      | RigidVar _
      | BVar _
      | Const _ -> ()
      | App (head, l) -> vars head; List.iter vars l
      | Record (l, rest) ->
          CCOpt.iter vars rest;
          List.iter (fun (s,t') -> vars t') l
      | RecordGet (r, _) -> vars r
      | RecordSet (r, _, sub) -> vars r; vars sub
      | SimpleApp (_,l)
      | Multiset l -> List.iter vars l
      | Bind (_, varty, t') -> vars varty; vars t'
      | At (l,r) -> vars l; vars r
    in
    vars t

  let subterms t k =
    let rec subterms t =
      k t;
      match view t with
      | Var _
      | RigidVar _
      | BVar _
      | Const _ -> ()
      | Bind (_, varty, t') -> subterms varty; subterms t'
      | Record (l, rest) ->
          CCOpt.iter subterms rest;
          List.iter (fun (_,t') -> subterms t') l
      | RecordGet (r, _) -> subterms r
      | RecordSet (r, _, sub) -> subterms r; subterms sub
      | SimpleApp (_, l)
      | Multiset l -> List.iter subterms l
      | App(f, l) -> subterms f; List.iter subterms l
      | At (l,r) -> subterms l; subterms r
    in
    subterms t

  let rigid_vars t =
    subterms t |> Sequence.filter is_rigid_var

  let subterms_depth t k =
    let rec recurse depth t =
      k (t, depth);
      match view t with
      | App (_,l) ->
        let depth' = depth + 1 in
        List.iter (fun t' -> recurse depth' t') l
      | Record (l,rest) ->
          begin match rest with
          | None -> () | Some r -> recurse (depth+1) r
          end;
          List.iter (fun (s,t') -> recurse depth t') l
      | RecordGet (r, _) -> recurse depth r
      | RecordSet (r, _, sub) -> recurse depth r; recurse depth sub
      | SimpleApp (_,l)
      | Multiset l -> List.iter (recurse (depth+1)) l
      | Bind (_, varty, t') -> recurse depth varty; recurse (depth+1) t'
      | At (l,r) -> recurse (depth+1) l; recurse (depth+1) r
      | Const _
      | Var _
      | BVar _
      | RigidVar _ -> ()
    in
    recurse 0 t

  let symbols t k =
    let rec symbols t = match view t with
      | Var _ | BVar _ | RigidVar _ -> ()
      | Const s -> k s
      | App (head, l) -> symbols head; List.iter symbols l
      | At (l,r) -> symbols l; symbols r
      | Record (l, rest) ->
          CCOpt.iter symbols rest;
          List.iter (fun (_,t') -> symbols t') l
      | Multiset l -> List.iter symbols l
      | SimpleApp (s,l) ->
        k s;
        List.iter symbols l
      | RecordGet (r, _) -> symbols r
      | RecordSet (r, _, sub) -> symbols r; symbols sub
      | Bind (s, varty, t') -> k s; symbols varty; symbols t'
    in
    symbols t

  let types t k =
    let rec types t =
      begin match t.ty with
      | NoType -> ()
      | HasType ty -> k ty
      end;
      match view t with
      | Var _ | BVar _ | RigidVar _ | Const _ -> ()
      | App (head, l) -> types head; List.iter types l
      | Record (l,rest) ->
          CCOpt.iter types rest;
          List.iter (fun (_,t') -> types t') l
      | RecordGet (r, _) -> types r
      | RecordSet (r, _, sub) -> types r; types sub
      | SimpleApp (_,l)
      | Multiset l -> List.iter types l
      | Bind (_, _, t') -> types t'
      | At (l,r) -> types l; types r
    in types t

  let max_var seq =
    let r = ref 0 in
    seq (fun t -> match view t with Var i -> r := max i !r | _ -> ());
    !r

  let min_var seq =
    let r = ref max_int in
    seq (fun t -> match view t with Var i -> r := min i !r | _ -> ());
    !r

  let add_set = Sequence.fold (fun set t -> Set.add t set)

  let add_tbl tbl = Sequence.iter (fun t -> Tbl.replace tbl t ())
end

(** {3 LogtkPositions} *)

module Pos = struct
  module P = LogtkPosition
  let rec at t pos = match view t, pos with
    | _, P.LogtkType pos' ->
        begin match t.ty with
        | NoType -> invalid_arg "wrong position: term has no type"
        | HasType ty -> at ty pos'
        end
    | _, P.Stop -> t
    | (Var _ | BVar _ | RigidVar _ ), _ -> invalid_arg "wrong position in term"
    | Bind(_, _, t'), P.Right subpos -> at t' subpos
    | App (t, _), P.Head subpos -> at t subpos
    | App (_, l), P.Arg (n,subpos) when n < List.length l ->
      at (List.nth l n) subpos
    | At (l,r), P.Left subpos -> at l subpos
    | At (l,r), P.Right subpos -> at r subpos
    | Multiset l, P.Arg (n,subpos) when n < List.length l ->
        at (List.nth l n) subpos
    | Record (_, Some r), P.Record_rest subpos ->
        at r subpos
    | Record (l, _), P.Record_field (name, subpos) ->
        begin try
          let t' = List.assoc name l in
          at t' subpos
        with Not_found ->
          raise (Invalid_argument ("wrong position: no such record field: " ^name))
        end
    | SimpleApp (_, l), P.Arg(n,subpos) when n < List.length l ->
        at (List.nth l n) subpos
    | _ -> invalid_arg
      (LogtkUtil.sprintf "position %a not valid in term" P.pp pos)

  let rec replace t pos ~by = match t.ty, view t, pos with
    | _, _, P.Stop -> by
    | NoType, _, P.LogtkType pos' -> invalid_arg "wrong position: term has no type"
    | HasType ty, _, P.LogtkType pos' ->
        let ty = replace ty pos' ~by in
        cast ~ty t
    | _, (Var _ | BVar _ | RigidVar _ ), _ -> invalid_arg "wrong position in term"
    | HasType ty, Bind(s, varty, t'), P.Right subpos ->
        bind ~kind:t.kind ~ty ~varty s (replace t' subpos ~by)
    | HasType ty, App (f, l), P.Head subpos ->
        app ~kind:t.kind ~ty (replace f subpos ~by) l
    | HasType ty, App (f, l), P.Arg (n,subpos) when n < List.length l ->
        let t' = replace (List.nth l n) subpos ~by in
        let l' = CCList.Idx.set l n t' in
        app ~kind:t.kind ~ty f l'
    | HasType ty, At (l,r), P.Left subpos ->
        mk_at ~kind:t.kind ~ty (replace l subpos ~by) r
    | HasType ty, At (l,r), P.Right subpos ->
        mk_at ~kind:t.kind ~ty l (replace r subpos ~by)
    | HasType ty, Multiset l, P.Arg (n,subpos) when n < List.length l ->
        let t' = replace (List.nth l n) subpos ~by in
        let l' = CCList.Idx.set l n t' in
        multiset ~kind:t.kind ~ty l'
    | HasType ty, SimpleApp (s,l), P.Arg (n,subpos) when n < List.length l ->
        let t' = replace (List.nth l n) subpos ~by in
        let l' = CCList.Idx.set l n t' in
        simple_app ~kind:t.kind ~ty s l'
    | HasType ty, Record (l, Some r), P.Record_rest subpos ->
        let rest = Some (replace r subpos ~by) in
        record ~kind:t.kind ~ty l ~rest
    | HasType ty, Record (l, rest), P.Record_field (name, subpos) ->
        begin try
          let t' = replace (List.assoc name l) subpos ~by in
          let l' = (name,t') :: (List.remove_assoc name l) in
          record ~kind:t.kind ~ty l' ~rest
        with Not_found ->
          raise (Invalid_argument ("wrong position: no such record field: " ^name))
        end
    | _ -> invalid_arg
      (LogtkUtil.sprintf "position %a not valid in term" P.pp pos)
end

(* [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.ty, view t with
  | _ when eq t old -> by
  | HasType ty, Bind (s, varty, t') ->
    bind ~kind:t.kind ~ty ~varty s (replace t' ~old ~by)
  | HasType ty, Record (l, rest) ->
      let rest = match rest with
      | None -> None
      | Some r -> Some (replace r ~old ~by)
      in
      let l = List.map (fun (s,t') -> s, replace t' ~old ~by) l in
      record ~kind:t.kind ~ty l ~rest
  | HasType ty, App (f, l) ->
    let f' = replace f ~old ~by in
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    app ~kind:t.kind ~ty f' l'
  | HasType ty, At(l,r) ->
    let l' = replace l ~old ~by and r' = replace r ~old ~by in
    at ~kind:t.kind ~ty l' r'
  | HasType ty, Multiset l ->
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    multiset ~kind:t.kind ~ty l'
  | HasType ty, SimpleApp (s,l) ->
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    simple_app ~kind:t.kind ~ty s l'
  | HasType ty, RecordGet (r, name) ->
    record_get ~kind:t.kind ~ty (replace r ~old ~by) name
  | HasType ty, RecordSet (r, name, sub) ->
    record_set ~kind:t.kind ~ty (replace r ~old ~by) name (replace sub ~old ~by)
  | NoType, _ -> t
  | _, (Var _ | BVar _ | RigidVar _ | Const _) -> t

(** {3 Variables} *)

let close_vars ~kind ~ty s t =
  let vars = Seq.add_set Set.empty (Seq.vars t) in
  bind_vars ~kind ~ty s (Set.elements vars) t

(** {3 Misc} *)

let rec size t = match view t with
  | Const _
  | Var _
  | RigidVar _
  | BVar _ -> 1
  | Bind (_, _, t') -> 1 + size t'
  | Record (l, rest) ->
      let s = match rest with | None -> 0 | Some r -> size r in
      List.fold_left (fun acc (_,t') -> acc + size t') (1+s) l
  | RecordGet (r,_ ) -> 1 + size r
  | RecordSet (r,_ , sub) -> 1 + size r + size sub
  | SimpleApp (_,l)
  | Multiset l -> List.fold_left (fun s t -> s+size t) 1 l
  | App (head, l) -> _size_list (1 + size head) l
  | At (l,r) -> size l + size r + 1
and _size_list acc l = match l with
  | [] -> acc
  | t::l' -> _size_list (acc + size t) l'

let depth t =
  Seq.subterms_depth t |> Sequence.map snd |> Sequence.fold max 0

let rec head t = match view t with
  | BVar _ | Var _ | RigidVar _ | Record _ | Multiset _
  | RecordGet _ | RecordSet _ -> None
  | SimpleApp (s, _)
  | Const s
  | Bind (s, _, _) -> Some s
  | App (h, _) -> head h
  | At (l,_) -> head l

module PB = LogtkPosition.Build

let rec _all_pos_rec f vars acc pb t = match view t with
  | Var _ | BVar _ | RigidVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | Const _ -> f acc t (PB.to_pos pb)
  | App (hd, tl) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    let acc = _all_pos_rec f vars acc (PB.head pb) hd in
    _all_pos_rec_list f vars acc pb tl 0
  | Record (l,rest) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    let _, acc = List.fold_left
      (fun (i,acc) (_, t') ->
        i+1, _all_pos_rec f vars acc (PB.arg i pb) t')
      (0, acc) l
    in acc
  | At (l,r) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    let acc = _all_pos_rec f vars acc (PB.left pb) l in
    _all_pos_rec f vars acc (PB.right pb) r
  | SimpleApp (_,l)
  | Multiset l ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec_list f vars acc pb l 0
  | RecordGet (r, _) ->
    let acc = f acc t (PB.to_pos pb) in
    _all_pos_rec f vars acc (PB.left pb) r
  | RecordSet (r, _, sub) ->
    let acc = f acc t (PB.to_pos pb) in
    let acc = _all_pos_rec f vars acc (PB.left pb) r in
    _all_pos_rec f vars acc (PB.right pb) sub
  | Bind (_, _, t') ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec f vars acc (PB.right pb) t'
and _all_pos_rec_list f vars acc pb l i = match l with
  | [] -> acc
  | t::l' ->
    let acc = _all_pos_rec f vars acc (PB.arg i pb) t in
    _all_pos_rec_list f vars acc pb l' (i+1)

let all_positions ?(vars=false) ?(pos=LogtkPosition.stop) t acc f =
  _all_pos_rec f vars acc (PB.of_pos pos) t

(** {3 IO} *)

type print_hook = int -> (Buffer.t -> t -> unit) -> Buffer.t -> t -> bool

let _hooks = ref []
let add_default_hook h = _hooks := h :: !_hooks

let pp_depth ?(hooks=[]) depth buf t =
  let rec _pp depth buf t =
    if List.exists (fun h -> h depth (_pp depth) buf t) hooks
    then () (* hook took control *)
    else _pp_root depth buf t; _pp_ty depth buf t
  and _pp_root depth buf t = match view t with
  | Var i -> Printf.bprintf buf "X%d" i
  | RigidVar i -> Printf.bprintf buf "Z%d" i
  | BVar i -> Printf.bprintf buf "Y%d" (depth-i-1)
  | Const s -> Sym.pp buf s
  | Bind (s, varty, t') ->
    Printf.bprintf buf "%a Y%d:%a. %a" Sym.pp s depth
      (_pp depth) varty (_pp_surrounded (depth+1)) t'
  | Record ([], None) ->
    Buffer.add_string buf "{}"
  | Record ([], Some r) ->
    Printf.bprintf buf "{ | %a}" (_pp depth) r
  | Record (l, None) ->
    Buffer.add_char buf '{';
    List.iteri
      (fun i (s, t') ->
        if i>0 then Buffer.add_string buf ", ";
        Printf.bprintf buf "%s: " s;
        _pp depth buf t')
      l;
    Buffer.add_char buf '}'
  | Record (l, Some r) ->
    Buffer.add_char buf '{';
    List.iteri
      (fun i (s, t') ->
        if i>0 then Buffer.add_string buf ", ";
        Printf.bprintf buf "%s: " s;
        _pp depth buf t')
      l;
    Printf.bprintf buf " | %a}" (_pp depth) r
  | RecordGet (r, name) ->
    Printf.bprintf buf "%a.%s" (_pp depth) r name
  | RecordSet (r, name,sub) ->
    Printf.bprintf buf "%a.%s <- %a" (_pp depth) r name (_pp depth) sub
  | Multiset l ->
    Printf.bprintf buf "{| %a |}" (LogtkUtil.pp_list (_pp depth)) l
  | SimpleApp (s, [a]) when Sym.is_prefix s ->
    Printf.bprintf buf "%a %a" Sym.pp s (_pp depth) a
  | SimpleApp (s, [a;b]) when Sym.is_infix s ->
    Printf.bprintf buf "(%a %a %a)" (_pp depth) a Sym.pp s (_pp depth) b
  | SimpleApp (s, l) ->
    Printf.bprintf buf "%a(%a)" Sym.pp s (LogtkUtil.pp_list (_pp depth)) l
  | At (l,r) ->
    _pp_surrounded depth buf l;
    Buffer.add_char buf ' ';
    _pp depth buf r
  | App (f, l) ->
    assert (l <> []);
    _pp_surrounded depth buf f;
    List.iter
      (fun t' ->
        Buffer.add_char buf ' '; _pp_surrounded depth buf t')
      l
  and _pp_ty depth buf t = match t.ty, view t with
    | HasType ty, (Var _ | BVar _) ->
      Printf.bprintf buf ":%a" (_pp_surrounded depth) ty
    | _ -> ()
  and _pp_surrounded depth buf t = match view t with
  | Bind _
  | At _
  | App _ ->
    Buffer.add_char buf '(';
    _pp depth buf t;
    Buffer.add_char buf ')'
  | _ -> _pp depth buf t
  in
  _pp depth buf t

let pp buf t = pp_depth ~hooks:!_hooks 0 buf t
let to_string t = LogtkUtil.on_buffer pp t
let fmt fmt t = Format.pp_print_string fmt (to_string t)

(* FIXME
let bij =
  let open Bij in
  let (!!!) = Lazy.force in
  fix
    (fun bij ->
      let bij_bind = lazy (triple !!!bij Sym.bij !!!bij) in
      let bij_var = lazy (pair !!!bij int_) in
      let bij_cst = lazy (pair !!!bij Sym.bij) in
      let bij_at = lazy (triple !!!bij !!!bij (list_ !!!bij)) in
      switch
        ~inject:(fun t -> match view t with
        | BVar i -> "bv", BranchTo (!!!bij_var, (ty t, i))
        | Var i -> "v", BranchTo (!!!bij_var, (ty t, i))
        | Const s -> "c", BranchTo (!!!bij_cst, (ty t, s))
        | Bind (s, t') -> "bind", BranchTo (!!!bij_bind, (ty t, s, t'))
        | App (t, l) -> "a", BranchTo (!!!bij_at, (ty t, t, l)))
        ~extract:(function
        | "bv" -> BranchFrom (!!!bij_var, fun (ty,i) -> bvar ~ty i)
        | "v" -> BranchFrom (!!!bij_var, fun (ty,i) -> var ~ty i)
        | "c" -> BranchFrom (!!!bij_cst, fun (ty,s) -> const ~ty s)
        | "bind" -> BranchFrom (!!!bij_bind, fun (ty,s,t') -> bind ~ty s t')
        | "a" -> BranchFrom (!!!bij_at, fun (ty, t, l) -> app ~ty t l)
        | _ -> raise (DecodingError "expected Term")))
*)
