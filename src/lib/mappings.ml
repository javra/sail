(****************************************************************************)
(*     Sail                                                                 *)
(*                                                                          *)
(*  Sail and the Sail architecture models here, comprising all files and    *)
(*  directories except the ASL-derived Sail code in the aarch64 directory,  *)
(*  are subject to the BSD two-clause licence below.                        *)
(*                                                                          *)
(*  The ASL derived parts of the ARMv8.3 specification in                   *)
(*  aarch64/no_vector and aarch64/full are copyright ARM Ltd.               *)
(*                                                                          *)
(*  Copyright (c) 2013-2021                                                 *)
(*    Kathyrn Gray                                                          *)
(*    Shaked Flur                                                           *)
(*    Stephen Kell                                                          *)
(*    Gabriel Kerneis                                                       *)
(*    Robert Norton-Wright                                                  *)
(*    Christopher Pulte                                                     *)
(*    Peter Sewell                                                          *)
(*    Alasdair Armstrong                                                    *)
(*    Brian Campbell                                                        *)
(*    Thomas Bauereiss                                                      *)
(*    Anthony Fox                                                           *)
(*    Jon French                                                            *)
(*    Dominic Mulligan                                                      *)
(*    Stephen Kell                                                          *)
(*    Mark Wassell                                                          *)
(*    Alastair Reid (Arm Ltd)                                               *)
(*                                                                          *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  This work was partially supported by EPSRC grant EP/K008528/1 <a        *)
(*  href="http://www.cl.cam.ac.uk/users/pes20/rems">REMS: Rigorous          *)
(*  Engineering for Mainstream Systems</a>, an ARM iCASE award, EPSRC IAA   *)
(*  KTF funding, and donations from Arm.  This project has received         *)
(*  funding from the European Research Council (ERC) under the European     *)
(*  Union’s Horizon 2020 research and innovation programme (grant           *)
(*  agreement No 789108, ELVER).                                            *)
(*                                                                          *)
(*  This software was developed by SRI International and the University of  *)
(*  Cambridge Computer Laboratory (Department of Computer Science and       *)
(*  Technology) under DARPA/AFRL contracts FA8650-18-C-7809 ("CIFV")        *)
(*  and FA8750-10-C-0237 ("CTSRD").                                         *)
(*                                                                          *)
(*  SPDX-License-Identifier: BSD-2-Clause                                   *)
(****************************************************************************)

(** Implementation of elaboration for mapping patterns

    The elaboration is somewhat complicated as we need to turn:

    {@sail[
    $[<completeness>] match x {
      <pat> => <expr1>,
      mapping(A) => <expr2>,
      <rest>
    }
    ]}

    into

    {@sail[
    let y = x in
    $[mapping_match] $[complete] match ($[complete] match y {
      <pat> => Some(<expr1>),
      z if mapping_forwards_matches(z) => $[complete] match $[mapping_guarded] mapping_forwards(z) {
        A => Some(<expr2>),
        _ => None(),
      },
      _ => None(),
    }) {
      Some(w) => w,
      None() => $[<completeness>] match y {
        <rest>
      },
    }
    ]}

    which is quite complicated. The [$[mapping_match]] attribute
    ensures the type checker can re-check the mapping despite
    the added option type.

    There are a few constraints that make this rewrite as tricky looking as it is:

    - We don't want to duplicate any expression (`<expr1>` and `<expr2>`).
    - We can't change the evaluation order, as this might change the
      observable side effects of the expression. This includes the order
      in which we evaluate `mapping_forwards` relative to all the other
      expressions.
    - The scope of all pattern bindings should be preserved.
    - The match can be embedded in any context (so we can't use `return`).
    - Mappings can be nested e.g. `m1(m2(m3(<pattern>)))`.

    If the mapping match is in a return position we can instead rewrite to

    {@sail[
    let y = x in {
      $[complete] match y {
        <pat> => return <expr1>,
        z if mapping_forwards_matches(z) => $[complete] match $[mapping_guarded] mapping_forwards(z) {
          A => return <expr2>,
          _ => (),
        },
        _ => (),
      };
      $[<completeness>] match y {
        <rest>
      }
    }
    ]}

    which avoids the nested match statements and option type. *)

open Ast
open Ast_util
open Type_check
open Rewriter

let remove_direction_attrs map_uannot (P_aux (aux, (l, annot))) =
  let remove_attrs = map_uannot (fun uannot -> remove_attribute "forwards" (remove_attribute "backwards" uannot)) in
  P_aux (aux, (l, remove_attrs annot))

(* This function extracts mapping patterns from the provided pattern
   (if any exists), replacing them with identifiers generated by the
   subst function.*)
let rec extract_mapping_pats map_uannot is_mapping subst (P_aux (aux, annot)) =
  match aux with
  | P_app (mapping, pats) when is_mapping mapping ->
      let id = subst () in
      (remove_direction_attrs map_uannot (P_aux (P_id id, annot)), [(id, P_aux (P_app (mapping, pats), annot))])
  | P_app (f, pats) ->
      let pats, found_mapping = extract_mapping_pats_list map_uannot is_mapping subst pats in
      (P_aux (P_app (f, pats), annot), found_mapping)
  | P_tuple pats ->
      let pats, found_mapping = extract_mapping_pats_list map_uannot is_mapping subst pats in
      (P_aux (P_tuple pats, annot), found_mapping)
  | P_list pats ->
      let pats, found_mapping = extract_mapping_pats_list map_uannot is_mapping subst pats in
      (P_aux (P_list pats, annot), found_mapping)
  | P_vector pats ->
      let pats, found_mapping = extract_mapping_pats_list map_uannot is_mapping subst pats in
      (P_aux (P_vector pats, annot), found_mapping)
  | P_vector_concat pats ->
      let pats, found_mapping = extract_mapping_pats_list map_uannot is_mapping subst pats in
      (P_aux (P_vector_concat pats, annot), found_mapping)
  | P_string_append pats ->
      let pats, found_mapping = extract_mapping_pats_list map_uannot is_mapping subst pats in
      (P_aux (P_string_append pats, annot), found_mapping)
  | P_typ (typ, pat) ->
      let pat, found_mapping = extract_mapping_pats map_uannot is_mapping subst pat in
      (P_aux (P_typ (typ, pat), annot), found_mapping)
  | P_not pat ->
      let pat, found_mapping = extract_mapping_pats map_uannot is_mapping subst pat in
      (P_aux (P_not pat, annot), found_mapping)
  | P_as (pat, id) ->
      let pat, found_mapping = extract_mapping_pats map_uannot is_mapping subst pat in
      (P_aux (P_as (pat, id), annot), found_mapping)
  | P_var (pat, typ_pat) ->
      let pat, found_mapping = extract_mapping_pats map_uannot is_mapping subst pat in
      (P_aux (P_var (pat, typ_pat), annot), found_mapping)
  | P_or (pat1, pat2) ->
      extract_mapping_pats_pair map_uannot is_mapping subst (fun p1 p2 -> P_aux (P_or (p1, p2), annot)) pat1 pat2
  | P_cons (pat1, pat2) ->
      extract_mapping_pats_pair map_uannot is_mapping subst (fun p1 p2 -> P_aux (P_cons (p1, p2), annot)) pat1 pat2
  | P_id id -> (P_aux (P_id id, annot), [])
  | P_wild -> (P_aux (P_wild, annot), [])
  | P_lit lit -> (P_aux (P_lit lit, annot), [])
  | P_vector_subrange (id, n, m) -> (P_aux (P_vector_subrange (id, n, m), annot), [])
  | P_struct (fpats, fwild) ->
      let fields, pats = List.split fpats in
      let pats, found_mapping = extract_mapping_pats_list map_uannot is_mapping subst pats in
      (P_aux (P_struct (List.combine fields pats, fwild), annot), found_mapping)

and extract_mapping_pats_list map_uannot is_mapping subst pats =
  let extracted = List.map (extract_mapping_pats map_uannot is_mapping subst) pats in
  let pats, found = List.split extracted in
  (pats, List.concat found)

and extract_mapping_pats_pair map_uannot is_mapping subst f pat1 pat2 =
  let pat1, found1 = extract_mapping_pats map_uannot is_mapping subst pat1 in
  let pat2, found2 = extract_mapping_pats map_uannot is_mapping subst pat2 in
  (f pat1 pat2, found1 @ found2)

(* Take the above function for extracting mapping patterns and apply
   it to a list of match arms (pexps), stopping at the first arm where
   we find any mapping patterns. *)
let rec split_arms map_uannot is_mapping subst prev_arms = function
  | (Pat_aux (Pat_exp (pat, exp), annot) as arm) :: arms -> begin
      match extract_mapping_pats map_uannot is_mapping subst pat with
      | _, [] -> split_arms map_uannot is_mapping subst (arm :: prev_arms) arms
      | pat, mappings -> (List.rev prev_arms, Some (Pat_aux (Pat_exp (pat, exp), annot), mappings, arms))
    end
  | (Pat_aux (Pat_when (pat, guard, exp), annot) as arm) :: arms -> begin
      match extract_mapping_pats map_uannot is_mapping subst pat with
      | _, [] -> split_arms map_uannot is_mapping subst (arm :: prev_arms) arms
      | pat, mappings -> (List.rev prev_arms, Some (Pat_aux (Pat_when (pat, guard, exp), annot), mappings, arms))
    end
  | [] -> (List.rev prev_arms, None)

let name_gen prefix =
  let counter = ref 0 in

  let fresh () =
    let name = mk_id (prefix ^ string_of_int !counter ^ "#") in
    incr counter;
    name
  in
  fresh

let wrap_some ~return_position exp =
  if return_position then mk_exp (E_return exp) else mk_exp (E_app (mk_id "Some", [exp]))

(* Take a arm like "<pat> => <exp>" and turn it into "<pat> => Some(<exp>)", or
   "<pat> => return <exp>" when in return position. *)
let some_arm ~return_position = function
  | Pat_aux (Pat_exp (pat, exp), annot) -> Pat_aux (Pat_exp (pat, wrap_some ~return_position exp), annot)
  | Pat_aux (Pat_when (pat, guard, exp), annot) -> Pat_aux (Pat_when (pat, guard, wrap_some ~return_position exp), annot)

(* Create an arm like "_ => None()" or "_ => ()" (when in return position) *)
let wildcard_arm ~return_position () =
  if return_position then mk_pexp (Pat_exp (mk_pat P_wild, mk_lit_exp L_unit))
  else mk_pexp (Pat_exp (mk_pat P_wild, mk_exp (E_app (mk_id "None", [mk_lit_exp L_unit]))))

let unwrap_some =
  mk_pexp (Pat_exp (mk_pat (P_app (mk_id "Some", [mk_pat (P_id (mk_id "result"))])), mk_exp (E_id (mk_id "result"))))

let none_pexp exp = mk_pexp (Pat_exp (mk_pat (P_app (mk_id "None", [mk_pat (P_lit (mk_lit L_unit))])), exp))

let remove_completeness_attribute uannot = uannot |> remove_attribute "incomplete" |> remove_attribute "complete"

let match_completeness c (E_aux (aux, (l, uannot))) =
  let uannot = uannot |> remove_completeness_attribute |> add_attribute (gen_loc l) c None in
  match aux with
  | E_match _ -> E_aux (aux, (l, uannot))
  | _ -> Reporting.unreachable l __POS__ "Non-match in match_completeness"

let match_complete = match_completeness "complete"
let match_incomplete = match_completeness "incomplete"

type 'a mapping_split_arms = {
  head_exp : 'a exp;  (** The expression being matched on *)
  before_arms : 'a pexp list;  (** The arms before the first arm containing a mapping pattern *)
  subst_arm : 'a pexp;  (** The arm containing a mapping pattern, with mappings replaced by substituted identifiers *)
  mappings : (id * 'a pat) list;  (** The replaced mapping patterns with their substituted identifiers *)
  after_arms : 'a pexp list;  (** The arms after the first arm containing a mapping pattern *)
}

let strip_mapping_split_arms (msa : tannot mapping_split_arms) : uannot mapping_split_arms =
  {
    head_exp = strip_exp msa.head_exp;
    before_arms = List.map strip_pexp msa.before_arms;
    subst_arm = strip_pexp msa.subst_arm;
    mappings = List.map (fun (id, pat) -> (id, strip_pat pat)) msa.mappings;
    after_arms = List.map strip_pexp msa.after_arms;
  }

type direction = Forwards | Backwards

let direction_to_string = function Forwards -> "forwards" | Backwards -> "backwards"

let mapping_direction l uannot =
  if Option.is_some (get_attribute "forwards" uannot) then Forwards
  else if Option.is_some (get_attribute "backwards" uannot) then Backwards
  else Reporting.unreachable l __POS__ "Mapping with no direction annotation found"

let mapping_function mapping direction = append_id mapping ("_" ^ direction_to_string direction)
let mapping_guard mapping direction = append_id mapping ("_" ^ direction_to_string direction ^ "_matches")

let rec conj_exp = function
  | [] -> mk_lit_exp L_true
  | [exp] -> exp
  | exp :: exps -> mk_exp (E_app (mk_id "and_bool", [exp; conj_exp exps]))

let tuple_exp = function [exp] -> exp | exps -> mk_exp (E_tuple exps)

let tuple_pat = function [pat] -> pat | pats -> mk_pat (P_tuple pats)

let rec mappings_match ~terminal ~return_position is_mapping subst mappings pexp =
  let handle_mapping (subst_id, P_aux (aux, (l, uannot))) =
    match aux with
    | P_app (mapping, [subpat]) ->
        let direction = mapping_direction l uannot in
        let mapping_fun_id = mapping_function mapping direction in
        let mapping_guard_id = mapping_guard mapping direction in
        let guarded_attr = add_attribute (gen_loc l) "mapping_guarded" None empty_uannot in
        ( E_aux (E_app (mapping_fun_id, [mk_exp (E_id subst_id)]), (gen_loc l, guarded_attr)),
          mk_exp ~loc:(gen_loc l) (E_app (mapping_guard_id, [mk_exp (E_id subst_id)])),
          subpat
        )
    | _ -> Reporting.unreachable l __POS__ "Non-mapping in mappings_match"
  in
  let pat, guard_opt, exp, (l, _) = destruct_pexp pexp in
  let mappings = List.map handle_mapping mappings in
  let head_exp = tuple_exp (List.map (fun (head_exp, _, _) -> head_exp) mappings) in
  let guard_exp = conj_exp (List.map (fun (_, guard_exp, _) -> guard_exp) mappings) in
  let subpat = tuple_pat (List.map (fun (_, _, subpat) -> subpat) mappings) in
  let match_exp =
    let arms =
      if terminal then [construct_pexp (subpat, guard_opt, exp, (gen_loc l, empty_uannot))]
      else
        [
          construct_pexp (subpat, guard_opt, wrap_some ~return_position exp, (gen_loc l, empty_uannot));
          wildcard_arm ~return_position ();
        ]
    in
    rewrite_match_untyped ~return_position is_mapping subst head_exp arms (gen_loc l, empty_uannot)
  in
  construct_pexp (pat, Some guard_exp, match_exp, (gen_loc l, empty_uannot))

and rewrite_arms ~return_position is_mapping subst msa (l, uannot) =
  let head_exp_tmp = mk_id "head_exp#" in
  let mmatch terminal = mappings_match ~terminal ~return_position is_mapping subst msa.mappings msa.subst_arm in
  let new_head_exp =
    mk_exp
      (E_match
         ( mk_exp (E_id head_exp_tmp),
           List.map (some_arm ~return_position) msa.before_arms @ [mmatch false; wildcard_arm ~return_position ()]
         )
      )
    |> match_complete
  in
  let outer_match =
    if return_position then (
      match msa.after_arms with
      | [] ->
          mk_exp
            (E_match (mk_exp (E_id head_exp_tmp), List.map (some_arm ~return_position) msa.before_arms @ [mmatch true]))
          |> match_incomplete
      | _ ->
          let after_match =
            rewrite_match_untyped ~return_position is_mapping subst (mk_exp (E_id head_exp_tmp)) msa.after_arms
              (l, uannot)
          in
          E_aux (E_block [new_head_exp; after_match], (l, remove_completeness_attribute uannot))
    )
    else (
      match msa.after_arms with
      | [] ->
          E_aux (E_match (new_head_exp, [unwrap_some]), (l, add_attribute Parse_ast.Unknown "mapping_match" None uannot))
          |> match_incomplete
      | _ ->
          let after_match =
            rewrite_match_untyped ~return_position is_mapping subst (mk_exp (E_id head_exp_tmp)) msa.after_arms
              (l, uannot)
          in
          E_aux
            ( E_match (new_head_exp, [unwrap_some; none_pexp after_match]),
              (l, add_attribute Parse_ast.Unknown "mapping_match" None uannot)
            )
          |> match_complete
    )
  in
  (* Make sure we don't generate a pointless `let head_exp# = head_exp# in ...` *)
  match msa.head_exp with
  | E_aux (E_id id, _) when string_of_id id = "head_exp#" -> outer_match
  | _ -> mk_exp (E_let (mk_letbind (mk_pat (P_id head_exp_tmp)) msa.head_exp, outer_match))

and rewrite_match_untyped ~return_position is_mapping subst head_exp arms (l, (uannot : uannot)) =
  match split_arms (fun x -> x) is_mapping subst [] arms with
  | before_arms, Some (subst_arm, mappings, after_arms) ->
      rewrite_arms ~return_position is_mapping subst
        { head_exp; before_arms; subst_arm; mappings; after_arms }
        (l, uannot)
  | _, None -> E_aux (E_match (head_exp, arms), (l, uannot))

and rewrite_match_typed ~return_position is_mapping subst head_exp arms (l, (tannot : tannot)) =
  match split_arms map_uannot is_mapping subst [] arms with
  | before_arms, Some (subst_arm, mappings, after_arms) ->
      let rewritten_match =
        rewrite_arms ~return_position is_mapping subst
          (strip_mapping_split_arms { head_exp; before_arms; subst_arm; mappings; after_arms })
          (l, untyped_annot tannot)
      in
      check_exp (env_of_tannot tannot) rewritten_match (typ_of_tannot tannot)
  | _, None -> E_aux (E_match (head_exp, arms), (l, tannot))

let rewrite_exp (aux, annot) =
  match aux with
  | E_match (head_exp, arms) ->
      let fresh = name_gen "mapping" in
      rewrite_match_typed ~return_position:false
        (fun m -> Env.is_mapping m (env_of_annot annot))
        fresh head_exp arms annot
  | _ -> E_aux (aux, annot)

let rewrite_ast ast =
  (* Before doing the mapping rewrite we add constant-width bitvector
     type annotations to any mappings, so we can guarantee the
     end-result of the rewrite is type checkable. *)
  let add_mapping_bitvector_types (aux, annot) =
    let env = env_of_annot annot in
    match aux with
    | P_app (mapping, pats) when Env.is_mapping mapping env ->
        let typ = typ_of_annot annot in
        begin
          match typ with
          | Typ_aux (Typ_app (f, [A_aux (A_nexp (Nexp_aux (Nexp_constant n, _)), _)]), _)
            when string_of_id f = "bitvector" ->
              P_aux (P_typ (typ, P_aux (P_app (mapping, pats), annot)), annot)
          | _ -> P_aux (P_app (mapping, pats), annot)
        end
    | _ -> P_aux (aux, annot)
  in
  let pat_alg = { id_pat_alg with p_aux = add_mapping_bitvector_types } in
  let ast = rewrite_ast_base { rewriters_base with rewrite_pat = (fun _ -> fold_pat pat_alg) } ast in
  let alg = { id_exp_alg with e_aux = rewrite_exp } in
  rewrite_ast_base { rewriters_base with rewrite_exp = (fun _ -> fold_exp alg) } ast
