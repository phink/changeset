open Base

module Api = struct

  module type S = sig

    type 'a label

    type 'm t = {
      find : 'a. 'a label -> 'm -> 'a;
      bind : 'a. 'a label -> 'a -> 'm -> 'm;
    }
  end

  module Make (M : T1) : S with type 'a label = 'a M.t = struct

    type 'a label = 'a M.t

    type 'm t = {
      find : 'a. 'a label -> 'm -> 'a;
      bind : 'a. 'a label -> 'a -> 'm -> 'm;
    }
  end
end

module type Initial = sig

  type 'a label

  type source

  module Api : Api.S with type 'a label = 'a label

  val eq_label : 'a label * 'b label -> ('a, 'b) Type_equal.equal option

  val sexp_of_label : 'a label -> Sexp.t

  val proj : 'a label -> source -> 'a

  val load : 'm Api.t -> 'm -> source

  val embed : 'm Api.t -> 'm -> source -> 'm

end

module TMap = struct

  module type S = sig

    type 'a key

    type t

    exception Key_not_found of string

    val empty : t

    val is_empty : t -> bool

    val mem : 'a key -> t -> bool

    val add : 'a key -> 'a -> t -> t

    val find_exn : 'a key -> t -> 'a

    val sexp_of_key : 'a key -> Sexp.t

  end

  module Make (I : Initial) : S
    with type 'a key = 'a I.label = struct

    let sexp_of_key = I.sexp_of_label

    type 'a key = 'a I.label

    module Key = struct

      type t = H : _ key -> t

      let compare a b = Poly.compare a b

      let sexp_of_t (H k) = I.sexp_of_label k

    end

    module Comp : Comparator.S with type t = Key.t = struct

      type t = Key.t

      include Comparator.Make(Key)

    end

    exception Key_not_found of string

    type binding = B : 'a key * 'a -> binding

    type t = (Key.t, binding, Comp.comparator_witness) Map.t

    let empty = Map.empty (module Comp)

    let is_empty = Map.is_empty

    let find_exn : type a. a key -> t -> a = fun k s ->
      match Map.find_exn s (Key.H k) with
      | B (k', v) -> match I.eq_label (k, k') with
        | None -> assert false
        | Some Type_equal.T -> v

    let add k v m = Map.set m ~key:(Key.H k) ~data:(B (k, v))

    let mem k m = Map.mem m (Key.H k)

  end
end

module type S = sig

  type source

  type 'a label

  type t

  type error

  exception Invalid_changes of t

  type binding

  val bind : 'a label -> 'a -> binding

  val ( => ) : 'a label -> 'a -> binding

  val error : 'a label -> string -> error

  val empty : t

  val is_empty : t -> bool

  val is_valid : t -> bool

  val from_record : source -> t

  val put_change : 'a label -> 'a -> t -> t

  val put_changes : binding list -> t -> t

  val put_error : error -> t -> t

  val mem_label : _ label -> t -> bool

  val mem_error : error -> t -> bool

  val find : 'a label -> t -> 'a option

  val find_exn : 'a label -> t -> 'a

  val apply : t -> (source, t) Base.Result.t

  val apply_exn : t -> source

  val show_errors : t -> string

  val validate_change : 'a label -> ('a -> string list) -> t -> t

  val require : 'a label -> ?message:string -> t -> t

  type length_opt = [ `is of int | `max of int | `min of int ]

  val validate_length : 'a label -> ('a -> int) -> length_opt list -> t -> t

  val validate_string_length : string label -> length_opt list -> t -> t

  val validate_array_length : ('a array) label -> length_opt list -> t -> t

  val validate_list_length : ('a list) label -> length_opt list -> t -> t

  type 'a ord_opt = [
    | `equal_to of 'a
    | `greater_than of 'a
    | `greater_than_or_equal_to of 'a
    | `less_than of 'a
    | `less_than_or_equal_to of 'a
    ]

  val validate : 'a label -> (module Comparator.S with type t = 'a) ->
    'a ord_opt list -> t -> t

  val validate_int : int label -> int ord_opt list -> t -> t

  val validate_float : float label -> float ord_opt list -> t -> t

  val validate_bool : bool label -> bool ord_opt list -> t -> t

  val validate_char : char label -> char ord_opt list -> t -> t

  val validate_string : string label -> string ord_opt list -> t -> t

  val validate_inclusion : 'a label -> ('a -> 'a -> bool) -> 'a list ->
    ?message:string -> t -> t

  val validate_exclusion : 'a label -> ('a -> 'a -> bool) -> 'a list ->
    ?message:string -> t -> t

  val validate_acceptance : bool label -> ?message:string -> t -> t

  val validate_format : string label -> Str.regexp -> ?message:string -> t -> t

end

module Make (I : Initial) : S
  with type source = I.source
   and type 'a label = 'a I.label = struct

  type 'a label = 'a I.label

  module TMap = TMap.Make(I)

  type error = E : _ I.label * string -> error

  type source = I.source

  type t = {
    errors : error list;
    changes : TMap.t;
  }

  exception Invalid_changes of t

  type hidden = H : _ label -> hidden

  exception Not_found of hidden

  let empty = {
    errors = [];
    changes = TMap.empty
  }

  let is_empty t = TMap.is_empty t.changes

  let error k v = E (k, v)

  let show_errors t =
    let errors =
      String.concat ~sep:";" @@
      List.map ~f:(fun (E (l, s)) ->
          Printf.sprintf "%S:%S" (Sexp.to_string (TMap.sexp_of_key l)) s
        ) t.errors
    in
    Printf.sprintf "{\n  \"errors\":{%s}\n}" errors

  let put_error e t = {
    t with errors = e :: t.errors
  }

  let mem_error e t = List.mem t.errors e ~equal:Poly.equal

  let mem_label l t = TMap.mem l t.changes

  let is_valid t = match t.errors with
    | [] -> true
    | _ -> false

  let api =
    let api_find l tmap =
      try TMap.find_exn l tmap
      with Caml.Not_found -> raise (Not_found (H l))
    in
    I.Api.{
      find = api_find;
      bind = TMap.add;
    }

  let apply t =
    if is_valid t then
      try
        Result.Ok (I.load api t.changes)
      with
        Not_found (H k) ->
        Result.Error (put_error (error k "is required") t)
    else
      Result.Error t

  let apply_exn t =
    if is_valid t then
      try
        I.load api t.changes
      with
        Not_found (H k) ->
        let new_t = put_error (error k "is required") t in
        raise (Invalid_changes new_t)
    else
      raise (Invalid_changes t)

  type binding = B : 'a label * 'a -> binding

  let bind k v = B (k, v)

  let ( => ) k v = B (k, v)

  let put_change label v t = {
    t with changes = api.bind label v t.changes
  }

  let put_changes labels t =
    List.fold_left ~init:t ~f:(fun acc (B (k, v)) ->
        put_change k v acc
      ) labels

  let find key t =
    try Some (TMap.find_exn key t.changes)
    with Caml.Not_found -> None

  let find_exn key t = TMap.find_exn key t.changes

  let from_record s = {
    errors = [];
    changes = I.embed api TMap.empty s
  }

  let validate_change label predicate t = match find label t with
    | None -> t
    | Some v ->
      let errors = List.fold_left ~f:(fun acc s ->
          error label s :: acc
        ) ~init:t.errors (predicate v)
      in
      {t with errors}

  let require l ?(message = "is required") t =
    if not (mem_label l t) then
      put_error (error l message) t
    else
      t

  type length_opt = [ `is of int | `max of int | `min of int ]

  let message_by_default = function
    | `is n ->
      Printf.sprintf "should be %d element(s)" n
    | `min n ->
      Printf.sprintf "should be at least %d element(s)" n
    | `max n ->
      Printf.sprintf "should be at most %d element(s)" n

  let validate_length label f options message t =
    let validate_length_aux length =
      let aux acc option = match option with
        | `is n when not (n = length) ->
          message option :: acc
        | `min n when length < n ->
          message option :: acc
        | `max n when length > n ->
          message option :: acc
        | _ ->
          acc
      in
      List.fold_left ~f:aux ~init:[] options
    in
    validate_change label (fun x -> validate_length_aux (f x)) t

  let validate_string_length label options t =
    let message = function
      | `is n ->
        Printf.sprintf "should be %d character(s)" n
      | `min n ->
        Printf.sprintf "should be at least %d character(s)" n
      | `max n ->
        Printf.sprintf "should be at most %d character(s)" n
    in
    validate_length label String.length options message t

  let validate_array_length label options t =
    validate_length label Array.length options message_by_default t

  let validate_list_length label options t =
    validate_length label List.length options message_by_default t

  let validate_length l f opts t = validate_length l f opts message_by_default t

  type 'a ord_opt =
    [ `equal_to of 'a
    | `greater_than of 'a
    | `greater_than_or_equal_to of 'a
    | `less_than of 'a
    | `less_than_or_equal_to of 'a
    ]

  let message_by_default option pp = match option with
    | `equal_to n ->
      Printf.sprintf "must be equal to %s" (pp n)
    | `less_than n ->
      Printf.sprintf "must be less than %s" (pp n)
    | `less_than_or_equal_to n ->
      Printf.sprintf "must be less than or equal to %s" (pp n)
    | `greater_than n ->
      Printf.sprintf "must be greater than %s" (pp n)
    | `greater_than_or_equal_to n ->
      Printf.sprintf "must be greater than or equal to %s" (pp n)

  let validate (type a) label (module M : Comparator.S with type t = a)  options t =
    let compare = M.comparator.compare in
    let pp x = Sexp.to_string (M.comparator.sexp_of_t x) in
    let aux x option = match option with
      | `equal_to n when compare n x <> 0 ->
        [message_by_default option pp]
      | `less_than n when compare x n <> -1 ->
        [message_by_default option pp]
      | `less_than_or_equal_to n when compare x n = 1 ->
        [message_by_default option pp]
      | `greater_than n when compare x n <> 1 ->
        [message_by_default option pp]
      | `greater_than_or_equal_to n when compare x n = -1 ->
        [message_by_default option pp]
      | _ ->
        []
    in
    validate_change label (fun x -> List.concat_map ~f:(aux x) options) t

  let validate_int label = validate label (module Int)

  let validate_float label = validate label (module Float)

  let validate_char label = validate label (module Char)

  let validate_string label = validate label (module String)

  let validate_bool label = validate label (module Bool)

  let validate_inclusion label equal values ?(message = "is invalid") t =
    let aux x = if List.mem ~equal values x then [] else [message] in
    validate_change label aux t

  let validate_exclusion label equal values ?(message = "is reserved") t =
    let aux x = if List.mem ~equal values x then [message] else [] in
    validate_change label aux t

  let validate_acceptance label ?(message = "must be accepted") t =
    validate_change label (fun x -> if not x then [message] else []) t

  let validate_format label regex ?(message = "has invalid format") t =
    let aux x = if not (Str.string_match regex x 0) then [message] else [] in
    validate_change label aux t

end
