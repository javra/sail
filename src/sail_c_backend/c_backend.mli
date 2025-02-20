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

open Libsail

open Jib
open Type_check

(** Global compilation options *)

(** Define generated functions as static *)
val opt_static : bool ref

(** Ordinarily we use plain z-encoding to name-mangle generated Sail
   identifiers into a form suitable for C. If opt_prefix is set, then
   the "z" which is added on the front of each generated C function
   will be replaced by opt_prefix. E.g. opt_prefix := "sail_" would
   give sail_my_function rather than zmy_function. *)
val opt_prefix : string ref

(** opt_extra_params and opt_extra_arguments allow additional state to
   be threaded through the generated C code by adding an additional
   parameter to each function type, and then giving an extra argument
   to each function call. For example we could have

   opt_extra_params := Some "CPUMIPSState *env"
   opt_extra_arguments := Some "env"

   and every generated function will take a pointer to a QEMU MIPS
   processor state, and each function will be passed the env argument
   when it is called. *)
val opt_extra_params : string option ref

val opt_extra_arguments : string option ref

(** Optimization flags *)

val optimize_primops : bool ref
val optimize_hoist_allocations : bool ref
val optimize_alias : bool ref
val optimize_fixed_int : bool ref
val optimize_fixed_bits : bool ref

module type CODEGEN_CONFIG = sig
  (** If this is true, then we will generate a separate header file,
      otherwise a single C file will be generated without a header
      file. *)
  val generate_header : bool

  (** A list of includes for the generated C file *)
  val includes : string list

  (** A list of includes for the generated header (if it is
      created). *)
  val header_includes : string list

  (** Do not generate a main function *)
  val no_main : bool

  (** Do not include sail.h automatically *)
  val no_lib : bool

  (** Do not include rts.h (the runtime), and do not generate code
      that requires any setup or teardown routines to be run by a runtime
      before executing any instruction semantics. *)
  val no_rts : bool

  (** Do not mangle generated C identifiers, prefer readable names
      when possible. *)
  val no_mangle : bool

  val reserved_words : Util.StringSet.t

  val overrides : string Name_generator.Overrides.t

  (** If [Some channel], the generated C code will be instrumented to
      track branch coverage information. Information about all the
      possible branches will be written to the provided output
      channel. *)
  val branch_coverage : out_channel option
end

module Codegen (Config : CODEGEN_CONFIG) : sig
  val jib_of_ast : Env.t -> Effects.side_effect_info -> typed_ast -> cdef list * Jib_compile.ctx
  val compile_ast : Env.t -> Effects.side_effect_info -> string -> typed_ast -> string option * string
end
