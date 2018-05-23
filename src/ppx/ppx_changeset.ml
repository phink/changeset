(* Copyright (c) 2018 Paul Laforgue <paul.laforgue123@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

open Base
open Ppxlib
open Ast_builder.Default

module Common = struct

  let source_type_name = "source"
  let label_type_name = "label"

  let cset_mod_name = "CSet"

  let changeset_mod_sig_lid = Longident.parse "Changeset.S"
  let changeset_functor_lid = Longident.parse "Changeset.Make"

  type t = {
    expression: expression;
    name: longident loc;
    pattern: pattern;
  }

  let t_of_ld ~loc ld =
    let uident = Located.lident ~loc (String.capitalize ld.pld_name.txt) in
    {
      expression = pexp_construct ~loc uident None;
      name = Located.lident ~loc ld.pld_name.txt;
      pattern = ppat_construct ~loc uident None;
    }

  let mk_ext_s name ty_ext_s =
    if String.equal ty_ext_s "t"
    then name
    else name ^ "_" ^ ty_ext_s

  let verify_at_least_one_record_not_parameterized ~loc tds =
    let is_not_parameterized_record td =
      match (td.ptype_kind, td.ptype_params) with
      | Ptype_record _, [] -> true
      | _ -> false
    in
    if not (List.exists ~f:is_not_parameterized_record tds) then
      Location.raise_errorf ~loc
        "'changeset' can only be applied on type definitions in which at \
         least one type definition is a record and is not parameterized"

  let gen_constructor ~loc ty_lid ld =
    let name = Located.mk ~loc (String.capitalize ld.pld_name.txt) in
    let args = Pcstr_tuple [] in
    let res = Some (ptyp_constr ~loc ty_lid [ld.pld_type]) in
    constructor_declaration ~loc ~name ~args ~res

  let gen_label_ext_type_td ~loc label_ext_s lds =
    let name = Located.mk ~loc label_ext_s in
    let ty_lid = Located.map_lident name in
    let cds = List.map ~f:(gen_constructor ~loc ty_lid) lds in
    let kind = Ptype_variant cds in
    let params = [(ptyp_any ~loc, Invariant)] in
    type_declaration
      ~name
      ~loc
      ~params
      ~cstrs:[]
      ~kind
      ~private_:Public
      ~manifest:None

  let with_type ~loc ~lid ~name ~params ~cstrs ~kind ~private_ ~manifest =
    let td =
      type_declaration
        ~loc
        ~name
        ~params
        ~cstrs
        ~kind
        ~private_
        ~manifest
    in
    Pwith_type (lid, td)

  let gen_mod_typ ~loc label_ext_s ty_ext_s =
    let source_cstr =
      let source_name = Located.mk ~loc source_type_name in
      let source_lid = Located.map_lident source_name in
      let ty_lid = Located.lident ~loc ty_ext_s in
      let manifest = Some (ptyp_constr ~loc ty_lid []) in
      with_type ~loc
        ~lid:source_lid
        ~name:source_name
        ~params:[]
        ~cstrs:[]
        ~manifest
        ~private_:Public
        ~kind:Ptype_abstract
    in
    let label_cstr =
      let label_name = Located.mk ~loc label_type_name in
      let label_lid = Located.map_lident label_name in
      let a' = ptyp_var ~loc "a" in
      let manifest =
        let lid = Located.lident ~loc label_ext_s in
        Some (ptyp_constr ~loc lid [a'])
      in
      let params = [(a', Invariant)] in
      with_type ~loc
        ~lid:label_lid
        ~name:label_name
        ~params
        ~cstrs:[]
        ~manifest
        ~private_:Public
        ~kind:Ptype_abstract
    in
    let changeset_s_lid = Located.mk ~loc changeset_mod_sig_lid in
    let changeset_cstr = pmty_ident ~loc changeset_s_lid in
    pmty_with ~loc changeset_cstr [source_cstr; label_cstr]

end

module Gen_structure = struct

  module C = Common

  let gen_label_ext_type ~loc label_ext_s lds =
    let td = C.gen_label_ext_type_td ~loc label_ext_s lds in
    pstr_type ~loc Recursive [td]

  let gen_source_type ~loc ty_ext_s =
    [%stri
      type source = [%t ptyp_constr ~loc (Located.lident ~loc ty_ext_s) []]
    ]

  let gen_label_type ~loc label_ext_s =
    let a' = ptyp_var ~loc "a" in
    let manifest = ptyp_constr ~loc (Located.lident ~loc label_ext_s) [a'] in
    [%stri
      type nonrec 'a label = [%t manifest]
    ]

  let gen_api_mod ~loc = [%stri
    module Api = Changeset.Api.Make(struct type 'a t = 'a label end)
  ]

  let gen_ty_eq ~loc labels =
    let failure = [%expr None] in
    let success = [%expr Some Base.Type_equal.T] in
    let tuple2 x = ppat_tuple ~loc [x; x] in
    let otherwise =
      let lhs = tuple2 (ppat_any ~loc) in
      case ~lhs ~rhs:failure ~guard:None
    in
    let init = if List.length labels > 1 then [otherwise] else [] in
    let cases = List.fold_left ~f:(fun acc label ->
        let lhs = tuple2 label.C.pattern in
        let case = case ~lhs ~rhs:success ~guard:None in
        case :: acc
      ) ~init labels
    in
    let body = pexp_function ~loc cases in
    [%stri
      let eq_label
        : type a b. a label * b label -> (a, b) Base.Type_equal.t option
        = [%e body]
    ]

  let gen_proj ~loc labels =
    let source = [%expr source] in
    let label = [%expr label] in
    let mk_case label =
      let rhs = pexp_field ~loc source label.C.name in
      case ~lhs:label.pattern ~rhs ~guard:None
    in
    let cases = List.map ~f:mk_case labels in
    let body = pexp_match ~loc label cases in
    [%stri
      let proj
        : type a. a label -> source -> a
        = fun label source -> [%e body]
    ]

  let gen_sexp_of_label ~loc labels =
    let mk_atom label =
      let s = estring ~loc (Longident.last_exn label.C.name.txt) in
      [%expr Base.Sexp.Atom [%e s]]
    in
    let mk_case label =
      let rhs = mk_atom label in
      case ~lhs:label.pattern ~rhs ~guard:None
    in
    let cases = List.map ~f:mk_case labels in
    let body = pexp_function ~loc cases in
    [%stri
      let sexp_of_label
        : type a. a label -> Base.Sexp.t
        = [%e body]
    ]

  let gen_load ~loc labels =
    let mk_find label = [%expr api.Api.find [%e label.C.expression] changes] in
    let mk_assoc label = (label.C.name, mk_find label) in
    let association = List.map ~f:mk_assoc labels in
    let body = pexp_record ~loc association None in
    [%stri
      let load api changes
        = [%e body]
    ]

  let gen_embed ~loc labels =
    let source = [%expr source] in
    let mk_field label = pexp_field ~loc source label.C.name in
    let aux acc label = [%expr
      api.Api.bind [%e label.C.expression] [%e mk_field label] [%e acc]
    ]
    in
    let body = List.fold_left ~f:aux ~init:[%expr empty] labels in
    [%stri
      let embed api empty source
        = [%e body]
    ]

  let gen_module ~loc label_ext_s ty_ext_s lds =
    let labels = List.map ~f:(C.t_of_ld ~loc) lds in
    let mod_arg = pmod_structure ~loc [
        gen_source_type ~loc ty_ext_s;
        gen_label_type ~loc label_ext_s;
        gen_api_mod ~loc;
        gen_proj ~loc labels;
        gen_ty_eq ~loc labels;
        gen_sexp_of_label ~loc labels;
        gen_embed ~loc labels;
        gen_load ~loc labels;
      ]
    in
    let mod_make =
      let lid = Located.mk ~loc C.changeset_functor_lid in
      pmod_ident ~loc lid
    in
    let mod_expr = pmod_apply ~loc mod_make mod_arg in
    let mod_ty = C.gen_mod_typ ~loc label_ext_s ty_ext_s in
    let name = Located.mk ~loc (C.mk_ext_s C.cset_mod_name ty_ext_s) in
    let mod_constr = pmod_constraint ~loc mod_expr mod_ty in
    pstr_module ~loc (module_binding ~loc ~name ~expr:mod_constr)

  let str_type_decl ~loc ~path:_ (_rec_flag, tds) =
    C.verify_at_least_one_record_not_parameterized ~loc tds;
    let derive = function
      | {ptype_kind = Ptype_record lds; _} as ty ->
        let ty_ext_s = ty.ptype_name.txt in
        let label_ext_s = C.mk_ext_s C.label_type_name ty_ext_s in
        [
          gen_label_ext_type ~loc label_ext_s lds;
          gen_module ~loc label_ext_s ty_ext_s lds;
        ]
      | _ ->
        []
    in
    List.concat_map ~f:derive tds

  let generate = Deriving.Generator.make_noarg str_type_decl

end

module Gen_signature = struct

  module C = Common

  let gen_label_ext_type ~loc label_ext_s lds =
    let td = C.gen_label_ext_type_td ~loc label_ext_s lds in
    psig_type ~loc Recursive [td]

  let gen_module_sig ~loc label_ext_s ty_ext_s =
    let name = Located.mk ~loc (C.mk_ext_s C.cset_mod_name ty_ext_s) in
    let mod_ty = C.gen_mod_typ ~loc label_ext_s ty_ext_s in
    let mod_decl = module_declaration ~loc ~name ~type_:mod_ty in
    psig_module ~loc mod_decl

  let sig_type_decl ~loc ~path:_ (_rec_flag, tds) =
    C.verify_at_least_one_record_not_parameterized ~loc tds;
    let derive = function
      | {ptype_kind = Ptype_record lds; ptype_params = []; _} as ty ->
        let ty_ext_s = ty.ptype_name.txt in
        let label_ext_s = C.mk_ext_s C.label_type_name ty_ext_s in
        [
          gen_label_ext_type ~loc label_ext_s lds;
          gen_module_sig ~loc label_ext_s ty_ext_s;
        ]
      | _ ->
        []
    in
    List.concat_map ~f:derive tds

  let generate = Deriving.Generator.make_noarg sig_type_decl

end

(* Registration. *)

let name = "changeset"

let changeset =
  Deriving.add
    name
    ~str_type_decl:Gen_structure.generate
    ~sig_type_decl:Gen_signature.generate
