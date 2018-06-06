open Data
open Data.Changeset

open Base

let array = [|1;2;3|]
let bool = true
let int = 123
let list = [1;2;3]
let string = "dummy"

let data = {
  array;
  bool;
  int;
  list;
  string;
}

(* - original changeset is valid *)

let changeset = from_record data

let%test _ = is_valid changeset

(* - empty and is_empty *)

let%test _ = is_empty empty

(* - is_valid *)

let%test _ =
  let invalid_cst =
    changeset
    |> put_change String "abc"
    |> validate_string_length String [`min 2]
  in is_valid invalid_cst

let%test _ =
  let invalid_cst =
    changeset
    |> put_change String ""
    |> validate_string_length String [`min 2]
  in not (is_valid invalid_cst)

(* - from_record, mem_label *)

let%test _ =
  let changeset = from_record data in
  mem_label Array changeset
  && mem_label Bool changeset
  && mem_label Int changeset
  && mem_label List changeset
  && mem_label String changeset

let%test _ =
  let open Poly in
  let changeset = from_record data in
  find_exn Array changeset = data.array
  && find_exn Bool changeset = data.bool
  && find_exn Int changeset = data.int
  && find_exn List changeset = data.list
  && find_exn String changeset = data.string

(* - put_change *)

let%test _ =
  let new_changeset = put_change Int 1 changeset in
  find_exn Int new_changeset = 1

let%test _ =
  let new_changeset = put_change Int 1 changeset in
  let new_changeset = put_change Int 2 new_changeset in
  find_exn Int new_changeset = 2

(* - put_changes *)

let%test _ =
  let new_changeset = put_changes [Int => 1; String => "new"] changeset in
  find_exn Int new_changeset = 1
  && String.equal (find_exn String new_changeset) "new"

let%test _ =
  let new_changeset = put_changes [Int => 1; Int => 2] changeset in
  find_exn Int new_changeset = 2

(* - put_error, error and mem_error *)

let%test _ =
  let e = error Int "dummy" in
  let changeset = put_error e empty in
  mem_error e changeset

(* - find and find_exn *)

let%test _ =
  let changeset = put_change Int 123 empty in
  Option.equal Int.equal (find Int changeset) (Some 123)

let%test _ =
  let changeset = put_change Int 123 empty in
  find_exn Int changeset = 123

let%test _ =
  let changeset = empty in
  match find_exn Int changeset with
  | exception Caml.Not_found -> true
  | _ -> false


(* - apply *)

let%test _ =
  match apply (from_record data) with
  | Error _ -> false
  | exception _ -> false
  | Ok new_data -> Poly.(new_data = data)

let%test _ =
  let changeset =
    put_changes
      [ Array => [||]
      (* ; Bool => true *)
      ; Int => 0
      ; List => []
      ; String => ""
      ] empty
  in
  match apply changeset with
  | Error cst ->
    mem_error (error Bool "is required") cst
  | Ok _ ->
    false

(* - apply_exn *)

let%test _ =
  match apply_exn (from_record data) with
  | exception _ -> false
  | new_data -> Poly.(new_data = data)

let%test _ =
  let changeset =
    put_changes
      [ Array => [||]
      (* ; Bool => true *)
      ; Int => 0
      ; List => []
      ; String => ""
      ] empty
  in
  match apply_exn changeset with
  | exception (Invalid_changes cst) ->
    mem_error (error Bool "is required") cst
  | _ ->
    false
