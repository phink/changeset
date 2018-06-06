type data = {
  array: int array;
  bool: bool;
  int: int;
  list: int list;
  string: string;
}

type 'a label =
  | Array : int array label
  | Bool : bool label
  | Int : int label
  | List : int list label
  | String : string label

module Changeset : Changeset_lib.S
  with type 'a label = 'a label
   and type source = data = Changeset_lib.Make(struct

    type source = data

    type nonrec 'a label = 'a label

    module Api = Changeset_lib.Api.Make(struct type 'a t = 'a label end)

    let sexp_of_label
      : type a. a label -> Base.Sexp.t
      = function
        | Array -> Base.Sexp.Atom "array"
        | Bool -> Base.Sexp.Atom "bool"
        | Int -> Base.Sexp.Atom "int"
        | List -> Base.Sexp.Atom "list"
        | String -> Base.Sexp.Atom "string"

    let eq_label
      : type a b. a label * b label -> (a, b) Base.Type_equal.t option
      = function
        | Array, Array -> Some Base.Type_equal.T
        | Bool, Bool -> Some Base.Type_equal.T
        | Int, Int -> Some Base.Type_equal.T
        | List, List -> Some Base.Type_equal.T
        | String, String -> Some Base.Type_equal.T
        | _ -> None

    let proj
      : type o. o label -> source -> o
      = fun label t ->
        match label with
        | Array -> t.array
        | Bool -> t.bool
        | Int -> t.int
        | List -> t.list
        | String -> t.string

    let load api changes = {
      array = api.Api.find Array changes;
      bool = api.Api.find Bool changes;
      int = api.Api.find Int changes;
      list = api.Api.find List changes;
      string = api.Api.find String changes;
    }

    let embed api empty t =
      empty
      |> api.Api.bind Array t.array
      |> api.Api.bind Bool t.bool
      |> api.Api.bind Int t.int
      |> api.Api.bind List t.list
      |> api.Api.bind String t.string
  end)
