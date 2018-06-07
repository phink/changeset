open Base

(** Consult the {{: ../index.html} homepage} of the project for
    getting started. *)

(** The module type of changesets and output signature of
    the functor {! Make}. *)

module type S = sig

  (** {3:types Types} *)

  type source
  (**
     The type of the original record type.
  *)

  type 'a label
  (**
     The type of the changeset labels corresponding to the fields defined in
     the type {! source} .
  *)

  type t
  (**
     The type of changesets.
  *)

  (** {3:exceptions Exceptions} *)

  exception Invalid_changes of t
  (**
     The exception raised when applying {! apply_exn} on an invalid changeset.
  *)

  (** {3:types Binding} *)

  type binding
  (**
     The type of bindings used by the {! put_changes} function.
  *)

  val bind : 'a label -> 'a -> binding
  (**
     [bind l x] returns a {! binding} of [l] to [x].
  *)

  val ( => ) : 'a label -> 'a -> binding
  (**
     [( => )] is an infix operator for the {! bind} function.
  *)

  (** {3:errors Errors} *)

  type error
  (**
     The type of errors returned when a validation is not respected.
  *)

  val error : 'a label -> string -> error
  (**
     [error l s] returns an {! error} associating the label [l] to
     the error message [s].
  *)

  (** {3:changeset Changeset} *)

  val empty : t
  (**
     The empty changeset.
  *)

  val is_empty : t -> bool
  (**
     [is_empty t] returns true if the changes is empty, false otherwise.
  *)

  val is_valid : t -> bool
  (**
     [is_valid t] returns [true] if the changeset [t] does not contain any error
     or [false] otherwise.
  *)

  val from_record : source -> t
  (**
     [from_record x] returns a changeset in which all the elements in [x]
     are loaded as changes.
  *)

  val put_change : 'a label -> 'a -> t -> t
  (**
     [put_change l x t] puts a change associating the label [l] with value [x]
     in the changeset [t].
  *)

  val put_changes : binding list -> t -> t
  (**
     [put_change bs t] applies {! put_change} to the list of binding [bs]
     in the changeset [t] from left to right.
  *)

  val put_error : error -> t -> t
  (**
     [put_error e t] adds the error [e] in the changeset [t].
  *)

  val mem_label : _ label -> t -> bool
  (**
     [mem_label l t] returns [true] if there is a binding of [l] in
     the changeset [t] or [false] otherwise.
  *)

  val mem_error : error -> t -> bool
  (**
     [mem_error e t] returns [true] if the error belongs to the changeset [t]
     or [false] otherwise.
  *)

  val find : 'a label -> t -> 'a option
  (**
     [find l t] returns the value associated with the label [l] in the
     changeset [t] wrapped in an option value.
     It returns [None] if the label [l] is unbound in [t].
  *)

  val find_exn : 'a label -> t -> 'a
  (**
     [find l t] returns the value associated with label [l] in the changeset [t].
     It raises the exception [Not_found] if the label [l] is unbound in [t].
  *)

  val apply : t -> (source, t) Result.t
  (**
     [apply t] returns:

     - [Ok x] where [x] is a value of type [source] if the changeset [t] is valid
     and all the labels are present in the changes of [t].
     - [Error t'] otherwise, where [t'] corresponds to the changeset [t] if
     it was not valid, or [t] expanded with an {! error} [(l => "is missing")]
     if the changeset was valid but a {! label} [l] was not found.
  *)

  val apply_exn : t -> source
  (**
     [apply t] returns a value of type {! source} if the changeset [t] is valid
     and all the labels are present in the changes of [t].
     It raises the exception {! Invalid_changes} otherwise.
  *)

  val show_errors : t -> string
  (**
     [show_errors t] returns a string in the json format of the errors
     contained in the changeset [t].
  *)

  (** {3:validations Validations} *)

  val validate_change : 'a label -> ('a -> string list) -> t -> t
  (**
     [validate_change l f t] applies [f] to the value associated with label
     [l] in the changeset [t] and adds the list of error messages returned
     to the errors of the changeset [t], all being indexed by label [l].
     If there is no binding for label [l] in changeset [t], the validation
     is not applied.
  *)

  val require : 'a label -> ?message:string -> t -> t
  (**
     [require l t] validates that there is a binding of [l] in
     the changeset [t].
     If not, the error [message] indexed by the label [l] is added
     to the changeset [t].
     Default error message is ["is required"].
  *)

  type length_opt = [
    | `is of int
    | `min of int
    | `max of int
  ]
  (** Options for length validations are the ones above where:

      - [`is n] : the length must be exactly [n].
      - [`min n] : the length must be greater than or equal to [n].
      - [`max n] : the length must be less than or equal to [n].

      Error messages for {! validate_length}, {! validate_array_length} and
      {! validate_list_length} are the following:

      - [`is n] :  ["should be %d element(s)"]
      - [`min n] : ["should be at least %d element(s)"]
      - [`max n] : ["should be at most %d element(s)"]

      Error messages for {! validate_string_length} are the following:

      - [`is n] :  ["should be %d character(s)"]
      - [`min n] : ["should be at least %d character(s)"]
      - [`max n] : ["should be at most %d character(s)"]

  *)

  val validate_length : 'a label -> ('a -> int) -> length_opt list -> t -> t
  (**
     [validate_length l f opts t] validates that the length  calculated
     by applying [f] to the value indexed by the label [l] in the changeset [t]
     respects the [opts].
  *)

  val validate_string_length : string label -> length_opt list -> t -> t
  (**
     [validate_string_length l opts t] validates that the length of the string
     indexed by the label [l] in the changeset [t] respects the [opts].
  *)

  val validate_array_length : ('a array) label -> length_opt list -> t -> t
  (**
     [validate_array_length l opts t] validates that the length of the array
     indexed by the label [l] in the changeset [t] respects the [opts].
  *)

  val validate_list_length : ('a list) label -> length_opt list -> t -> t
  (**
     [validate_list_length l opts t] validates that the length of the list
     indexed by the label [l] in the changeset [t] respects the [opts].
  *)

  type 'a ord_opt = [
    | `equal_to of 'a
    | `greater_than of 'a
    | `greater_than_or_equal_to of 'a
    | `less_than of 'a
    | `less_than_or_equal_to of 'a
    ]

  (** Options for ordered validations are the ones above where:

      - [`equal_to] : the indexed value must be equal to this value.
      - [`greater_than] : the indexed value must be strictly greater than
      this value.
      - [`greater_than_or_equal_to] : the indexed value must be greater than
      or equal to this value.
      - [`less_than] : the indexed value must be strictly less than this value.
      - [`less_than_or_equal_to] : the indexed value must be less than
      or equal to this value.

      Error messages are the following:
      - [`equal_to] : ["must be equal to %s"]
      - [`greater_than] : ["must be greater to %s"]
      - [`greater_than_or_equal_to] : ["must be greater than or equal to %s"]
      - [`less_than] : ["must be less than %s"]
      - [`less_than_or_equal_to] : ["must be less than or equal to %s"]

  *)

  val validate : 'a label -> (module Comparator.S with type t = 'a) ->
    'a ord_opt list -> t -> t
  (**
     [validate l cmp opts t] validates that the value indexed
     by the label [l] in the changeset [t] respects the [opts] using
     the [compare] function provided by the first-class module [cmp].
  *)

  val validate_int : int label -> int ord_opt list -> t -> t
  (**
     [validate_int l opts t] validates that the integer indexed by label [l]
     in the changeset [t] respects the [opts].
  *)

  val validate_float : float label -> float ord_opt list -> t -> t
  (**
     [validate_float l opts t] validates that float indexed by label [l]
     in the changeset [t] respects the [opts].
  *)

  val validate_bool : bool label -> bool ord_opt list -> t -> t
  (**
     [validate_bool l opts t] validates that the boolean indexed by label [l]
     in the changeset [t] respects the [opts].
  *)

  val validate_char : char label -> char ord_opt list -> t -> t
  (**
     [validate_char l opts t] validates that the char indexed by label [l]
     in the changeset [t] respects the [opts].
  *)

  val validate_string : string label -> string ord_opt list -> t -> t
  (**
     [validate_string l opts t] validates that the string indexed by label [l]
     in the changeset [t] respects the [opts].
  *)

  val validate_inclusion : 'a label -> ('a -> 'a -> bool) -> 'a list ->
    ?message:string -> t -> t
  (**
     [validate_inclusion equal l xs ?message t] validates that the value
     indexed by label [l] in the changeset [t] is a an included in [xs].
     If not, an error [message] indexed by the label [l] is added
     to the changeset [t].
     Default error message is ["is invalid"].
  *)

  val validate_exclusion : 'a label -> ('a -> 'a -> bool) -> 'a list ->
    ?message:string -> t -> t
  (**
     [validate_excluded equal l xs ?message t] validates that the value indexed
     by label [l] in the changeset [t] is not included in [xs].
     If not, an error [message] indexed by the label [l] is added
     to the changeset [t].
     Default error message is ["is reserved"].
  *)

  val validate_acceptance : bool label -> ?message:string -> t -> t
  (**
     [validate_string l ?message t] validates that the change indexed
     by label [l] in the changeset [t] is [true].
     If not, an error [message] indexed by the label [l] is added
     to the changeset [t].
     Default error message is ["must be accepted"].
  *)

  val validate_format : string label -> Str.regexp -> ?message:string -> t -> t
  (**
     [validate_format l reg ?message t] validates that the change indexed
     by label [l] in the changeset [t] is accepted by the regex [r].
     If not, an error [message] indexed by the label [l] is added
     to the changeset [t].
     Default error message is ["has invalid format"].
  *)

end

(** The module type of the API. *)

module Api : sig

  module type S = sig

    type 'a label

    type 'm t = {
      find : 'a. 'a label -> 'm -> 'a;
      bind : 'a. 'a label -> 'a -> 'm -> 'm;
    }
  end

  module Make : functor (M : T1) -> S with type 'a label = 'a M.t

end

(** The module type of initial configurations. *)

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

(** Functor for changesets. *)

module Make : functor (I : Initial) -> S
  with type 'a label = 'a I.label
   and type source = I.source
