open Data
open Data.CSet

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

(* - validate_change *)

let boot n f =
  changeset
  |> put_changes [Int => n]
  |> validate_change Int f

let%test _ =
  let cst = boot 0 (function
      | 123 -> []
      | _ -> ["dummy1"; "dummy2"]
    )
  in
  mem_error (error Int "dummy1") cst
  && mem_error (error Int "dummy2") cst

let%test _ =
  let cst = boot 123 (function
      | 123 -> []
      | _ -> ["dummy1"; "dummy2"]
    )
  in
  is_valid cst

(* - LENGTH *)

(* - validate_string_length *)

let boot n opts =
  changeset
  |> put_changes [String => String.init n (fun _ -> 'a')]
  |> validate_string_length String opts

(* `min *)

let%test _ =
  mem_error
    (error String "should be at least 2 character(s)")
    (boot 1 [`min 2])

let%test _ =
  is_valid
    (boot 2 [`min 2])

let%test _ =
  is_valid
    (boot 3 [`min 2])

(* `is *)

let%test _ =
  mem_error
    (error String "should be 2 character(s)")
    (boot 1 [`is 2])

let%test _ =
  is_valid
    (boot 2 [`is 2])

let%test _ =
  mem_error
    (error String "should be 2 character(s)")
    (boot 3 [`is 2])

(* `max *)

let%test _ =
  is_valid
    (boot 1 [`max 2])

let%test _ =
  is_valid
    (boot 2 [`max 2])

let%test _ =
  mem_error
    (error String "should be at most 2 character(s)")
    (boot 3  [`max 2])

(* `min and `max *)

let%test _ =
  mem_error
    (error String "should be at least 1 character(s)")
    (boot 0  [`min 1; `max 2])

let%test _ =
  is_valid
    (boot 1 [`min 1; `max 2])

let%test _ =
  is_valid
    (boot 2 [`min 1; `max 2])

let%test _ =
  mem_error
    (error String "should be at most 2 character(s)")
    (boot 3  [`min 1; `max 2])


(* - validate_list_length *)

let boot n opts =
  changeset
  |> put_changes [List => List.init n (fun _ -> 0)]
  |> validate_list_length List opts

(* `min *)

let%test _ =
  mem_error
    (error List "should be at least 2 element(s)")
    (boot 1  [`min 2])

let%test _ =
  is_valid
    (boot 2 [`min 2])

let%test _ =
  is_valid
    (boot 3 [`min 2])

(* `is *)

let%test _ =
  mem_error
    (error List "should be 2 element(s)")
    (boot 1 [`is 2])

let%test _ =
  is_valid
    (boot 2 [`is 2])

let%test _ =
  mem_error
    (error List "should be 2 element(s)")
    (boot 3 [`is 2])

(* `max *)

let%test _ =
  is_valid
    (boot 1 [`max 2])

let%test _ =
  is_valid
    (boot 2 [`max 2])

let%test _ =
  mem_error
    (error List "should be at most 2 element(s)")
    (boot 3  [`max 2])

(* `min and `max *)

let%test _ =
  mem_error
    (error List "should be at least 1 element(s)")
    (boot 0  [`min 1; `max 2])

let%test _ =
  is_valid
    (boot 1 [`min 1; `max 2])

let%test _ =
  is_valid
    (boot 2 [`min 1; `max 2])

let%test _ =
  mem_error
    (error List "should be at most 2 element(s)")
    (boot 3  [`min 1; `max 2])

(* - validate_array_length *)

let boot n opts =
  changeset
  |> put_changes [Array => Array.init n (fun _ -> 0)]
  |> validate_array_length Array opts

(* `min *)

let%test _ =
  mem_error
    (error Array "should be at least 2 element(s)")
    (boot 1 [`min 2])

let%test _ =
  is_valid
    (boot 2 [`min 2])

let%test _ =
  is_valid
    (boot 3 [`min 2])

(* `is *)

let%test _ =
  mem_error
    (error Array "should be 2 element(s)")
    (boot 1 [`is 2])

let%test _ =
  is_valid
    (boot 2 [`is 2])

let%test _ =
  mem_error
    (error Array "should be 2 element(s)")
    (boot 3 [`is 2])

(* `max *)

let%test _ =
  is_valid
    (boot 1 [`max 2])

let%test _ =
  is_valid
    (boot 2 [`max 2])

let%test _ =
  mem_error
    (error Array "should be at most 2 element(s)")
    (boot 3 [`max 2])

(* `min and `max *)

let%test _ =
  mem_error
    (error Array "should be at least 1 element(s)")
    (boot 0 [`min 1; `max 2])

let%test _ =
  is_valid
    (boot 1 [`min 1; `max 2])

let%test _ =
  is_valid
    (boot 2 [`min 1; `max 2])

let%test _ =
  mem_error
    (error Array "should be at most 2 element(s)")
    (boot 3 [`min 1; `max 2])

(* - ORDER *)

(* - validate_int *)

let boot n opts =
  changeset
  |> put_changes [Int => n]
  |> validate_int Int opts

(* `equal_to *)

let%test _ =
  mem_error
    (error Int "must be equal to 1")
    (boot 0 [`equal_to 1])

let%test _ =
  is_valid
    (boot 1 [`equal_to 1])

(* `greater_than *)

let%test _ =
  mem_error
    (error Int "must be greater than 0")
    (boot 0 [`greater_than 0])

let%test _ =
  is_valid
    (boot 1 [`greater_than 0])

(* `greater_than_or_equal_to *)

let%test _ =
  mem_error
    (error Int "must be greater than or equal to 1")
    (boot 0 [`greater_than_or_equal_to 1])

let%test _ =
  is_valid
    (boot 1 [`greater_than_or_equal_to 1])

let%test _ =
  is_valid
    (boot 2 [`greater_than_or_equal_to 1])

(* `less_than *)

let%test _ =
  is_valid
    (boot 0 [`less_than 1])

let%test _ =
  mem_error
    (error Int "must be less than 1")
    (boot 1 [`less_than 1])

let%test _ =
  mem_error
    (error Int "must be less than 1")
    (boot 2 [`less_than 1])

(* `less_than_or_equal_to *)

let%test _ =
  is_valid
    (boot 0 [`less_than_or_equal_to 1])

let%test _ =
  is_valid
    (boot 1 [`less_than_or_equal_to 1])

let%test _ =
  mem_error
    (error Int "must be less than or equal to 1")
    (boot 2 [`less_than_or_equal_to 1])

(* `greater_than and `less_than *)

let%test _ =
  mem_error
    (error Int "must be greater than 0")
    (boot 0 [`greater_than 0; `less_than 2])

let%test _ =
  is_valid
    (boot 1 [`greater_than 0; `less_than 2])

let%test _ =
  mem_error
    (error Int "must be less than 2")
    (boot 2 [`greater_than 0; `less_than 2])

(* - validate_acceptance *)

let boot ?message b =
  changeset
  |> put_changes [Bool => b]
  |> validate_acceptance Bool ?message

let%test _ =
  is_valid
    (boot true)

let%test _ =
  mem_error
    (error Bool "must be accepted")
    (boot false)

let%test _ =
  mem_error
    (error Bool "dummy")
    (boot false ~message:"dummy")

(* - validate_inclusion *)

let boot ?message s xs =
  changeset
  |> put_changes [String => s]
  |> validate_inclusion String String.equal xs ?message

let%test _ =
  is_valid
    (boot "b" ["a"; "b"; "c"])

let%test _ =
  mem_error
    (error String "is invalid")
    (boot "d" ["a"; "b"; "c"])

let%test _ =
  mem_error
    (error String "dummy")
    (boot "d" ["a"; "b"; "c"] ~message:"dummy")

(* - validate_exclusion *)

let boot ?message s xs =
  changeset
  |> put_changes [String => s]
  |> validate_exclusion String String.equal xs ?message

let%test _ =
  is_valid
    (boot "d" ["a"; "b"; "c"])

let%test _ =
  mem_error
    (error String "is reserved")
    (boot "b" ["a"; "b"; "c"])

let%test _ =
  mem_error
    (error String "dummy")
    (boot "b" ["a"; "b"; "c"] ~message:"dummy")

(* - validate_format *)

let boot ?message s =
  changeset
  |> put_changes [String => s]
  |> validate_format String (Str.regexp "[0-9]+") ?message

let%test _ =
  is_valid
    (boot "123")

let%test _ =
  mem_error
    (error String "has invalid format")
    (boot "")

let%test _ =
  mem_error
    (error String "dummy")
    (boot "" ~message:"dummy")
