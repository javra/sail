/*==========================================================================*/
/*     Sail                                                                 */
/*                                                                          */
/* Copyright 2024 Intel Corporation                                         */
/*   Pan Li - pan2.li@intel.com                                             */
/*                                                                          */
/*  SPDX-License-Identifier: BSD-2-Clause                                   */
/*==========================================================================*/

default Order dec

$include <prelude.sail>
$include <float/ne.sail>
$include "tuple_equality.sail"
$include "data.sail"

function test_float_is_ne () -> unit = {
  /* Half floating point */
  assert(float_is_ne((fp16_pos_denormal_0, fp16_pos_denormal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp16_neg_normal_0, fp16_neg_normal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp16_pos_denormal_1, fp16_pos_zero)) == (true, fp_eflag_none));
  assert(float_is_ne((fp16_pos_inf, fp16_neg_inf)) == (true, fp_eflag_none));

  assert(float_is_ne((fp16_pos_denormal_0, fp16_pos_denormal_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp16_neg_qnan_0, fp16_neg_qnan_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp16_pos_zero, fp16_neg_zero)) == (false, fp_eflag_none));
  assert(float_is_ne((fp16_pos_inf, fp16_pos_inf)) == (false, fp_eflag_none));

  assert(float_is_ne((fp16_pos_snan_0, fp16_pos_snan_0)) == (false, fp_eflag_invalid));
  assert(float_is_ne((fp16_pos_snan_1, fp16_pos_normal_0)) == (false, fp_eflag_invalid));

  /* Single floating point */
  assert(float_is_ne((fp32_pos_denormal_0, fp32_pos_denormal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp32_neg_normal_0, fp32_neg_normal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp32_pos_denormal_1, fp32_pos_zero)) == (true, fp_eflag_none));
  assert(float_is_ne((fp32_pos_inf, fp32_neg_inf)) == (true, fp_eflag_none));

  assert(float_is_ne((fp32_pos_denormal_0, fp32_pos_denormal_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp32_neg_qnan_0, fp32_neg_qnan_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp32_pos_zero, fp32_neg_zero)) == (false, fp_eflag_none));
  assert(float_is_ne((fp32_pos_inf, fp32_pos_inf)) == (false, fp_eflag_none));

  assert(float_is_ne((fp32_pos_snan_0, fp32_pos_snan_0)) == (false, fp_eflag_invalid));
  assert(float_is_ne((fp32_pos_snan_1, fp32_pos_normal_0)) == (false, fp_eflag_invalid));

  /* Double floating point */
  assert(float_is_ne((fp64_pos_denormal_0, fp64_pos_denormal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp64_neg_normal_0, fp64_neg_normal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp64_pos_denormal_1, fp64_pos_zero)) == (true, fp_eflag_none));
  assert(float_is_ne((fp64_pos_inf, fp64_neg_inf)) == (true, fp_eflag_none));

  assert(float_is_ne((fp64_pos_denormal_0, fp64_pos_denormal_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp64_neg_qnan_0, fp64_neg_qnan_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp64_pos_zero, fp64_neg_zero)) == (false, fp_eflag_none));
  assert(float_is_ne((fp64_pos_inf, fp64_pos_inf)) == (false, fp_eflag_none));

  assert(float_is_ne((fp64_pos_snan_0, fp64_pos_snan_0)) == (false, fp_eflag_invalid));
  assert(float_is_ne((fp64_pos_snan_1, fp64_pos_normal_0)) == (false, fp_eflag_invalid));

  /* Quad floating point */
  assert(float_is_ne((fp128_pos_denormal_0, fp128_pos_denormal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp128_neg_normal_0, fp128_neg_normal_1)) == (true, fp_eflag_none));
  assert(float_is_ne((fp128_pos_denormal_1, fp128_pos_zero)) == (true, fp_eflag_none));
  assert(float_is_ne((fp128_pos_inf, fp128_neg_inf)) == (true, fp_eflag_none));

  assert(float_is_ne((fp128_pos_denormal_0, fp128_pos_denormal_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp128_neg_qnan_0, fp128_neg_qnan_0)) == (false, fp_eflag_none));
  assert(float_is_ne((fp128_pos_zero, fp128_neg_zero)) == (false, fp_eflag_none));
  assert(float_is_ne((fp128_pos_inf, fp128_pos_inf)) == (false, fp_eflag_none));

  assert(float_is_ne((fp128_pos_snan_0, fp128_pos_snan_0)) == (false, fp_eflag_invalid));
  assert(float_is_ne((fp128_pos_snan_1, fp128_pos_normal_0)) == (false, fp_eflag_invalid));
}

function main () -> unit = {
  test_float_is_ne();
}
