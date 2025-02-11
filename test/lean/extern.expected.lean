import Out.Sail.Sail

open Sail

abbrev SailM := PreSailM PEmpty.elim trivialChoiceSource

def extern_add (_ : Unit) : Int :=
  (HAdd.hAdd 5 4)

def extern_sub (_ : Unit) : Int :=
  (HSub.hSub 5 (-4))

def extern_sub_nat (_ : Unit) : Nat :=
  (HSub.hSub 5 4)

def extern_negate (_ : Unit) : Int :=
  (Neg.neg 5)

def extern_mult (_ : Unit) : Int :=
  (HMul.hMul 5 4)

def extern_tdiv (_ : Unit) : Int :=
  (Int.tdiv 5 4)

def extern_tmod (_ : Unit) : Int :=
  (Int.tmod 5 4)

def extern_tmod_positive (_ : Unit) : Int :=
  (Int.tmod 5 4)

def extern_max (_ : Unit) : Int :=
  (Max.max 5 4)

def extern_min (_ : Unit) : Int :=
  (Min.min 5 4)

def extern_abs_int_plain (_ : Unit) : Int :=
  let x : Int := (-5)
  (Sail.Int.intAbs x)

def extern_eq_unit (_ : Unit) : Bool :=
  (Eq () ())

def extern_eq_bit (_ : Unit) : Bool :=
  (Eq 0#1 1#1)

def extern_not (_ : Unit) : Bool :=
  (Bool.not true)

def extern_and (_ : Unit) : Bool :=
  (Bool.and true false)

def extern_and_no_flow (_ : Unit) : Bool :=
  (Bool.and true false)

def extern_or (_ : Unit) : Bool :=
  (Bool.or true false)

def extern_eq_bool (_ : Unit) : Bool :=
  (Eq true false)

def extern_eq_int (_ : Unit) : Bool :=
  (Eq 5 4)

def extern_lteq_int (_ : Unit) : Bool :=
  (LE.le 5 4)

def extern_gteq_int (_ : Unit) : Bool :=
  (GE.ge 5 4)

def extern_lt_int (_ : Unit) : Bool :=
  (LT.lt 5 4)

def extern_gt_int (_ : Unit) : Bool :=
  (GT.gt 5 4)

def extern_eq_anything (_ : Unit) : Bool :=
  (BEq.beq true true)

def extern_vector_update (_ : Unit) : (Vector Int 5) :=
  (vectorUpdate #v[23, 23, 23, 23, 23] 2 42)

def initialize_registers (_ : Unit) : Unit :=
  ()

