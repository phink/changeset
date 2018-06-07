*This is WIP.*

| Service | OCaml | Service |
| ------- | ----- | ------- |
| Circle  | 4.06  |[![Build Status](https://circleci.com/gh/phink/changeset/tree/develop.svg?style=svg)](https://circleci.com/gh/phink/changeset/tree/develop) |

# Changeset

**Data validation with first-class and first-order labels in OCaml.**

Homepage for the library is
[available here](https://phink.github.io/changeset/changeset/index.html).

Changesets are tools to validate data while accumulating errors along the way.
They are composed of:

- **Changes**: an heterogeneous map whose keys are of type `'a label` and
values `'a`.
When deriving a changeset from a record type definition, the labels
correspond to the fields of this one.

- **Errors**: a list of strings associated with labels.

Labels are first-class, that means you can give them as argument to functions,
and first-order, so you can pattern match against them. The library
promotes the pipeline design approach.

There is a ppx deriving plugin so there is no need to write any
boilerplate code.

#### Install using opam
```
$ opam pin add changeset_lib https://github.com/phink/changeset.git
$ opam pin add ppx_changeset https://github.com/phink/changeset.git
```

#### Update the libraries and preprocess section in your jbuild file:

```
(libraries (... changeset))
(preprocess (pps (... ppx_changeset)))
```

### Minimal example

```ocaml
type t = {
  age: int;
  phone: string;
  password: string;
} [@@deriving changeset]

let validate cset =
  cset
  |> Changeset.validate_int Age [`greater_than_or_equal_to 0]
  |> Changeset.validate_string_length Password [`min 12]
  |> Changeset.validate_format Phone (Str.regexp "^\\+?[1-9][0-9]+$")

let create t =
  let cset = Changeset.from_record t in
  Changeset.apply (validate cset)

let data = {age = 12; password = "dolphin"; phone = "+14155552671"}

let () = match create data with
  | Ok t -> (* do what you want *)
  | Error cset ->
    Stdio.prerr_endline (Changeset.show_errors cset)
```

Execution

```json
{
  "errors":{
    "password":"should be at least 12 character(s)"
  }
}
```

### Limitations

Parameterized source types are not yet supported.
