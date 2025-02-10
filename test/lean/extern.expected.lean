import Out.Sail.Sail

open Sail

abbrev SailM := PreSailM PEmpty.elim trivialChoiceSource

def extern_add (_ : Unit) : Int :=
  (Int.add 5 4)

def extern_sub (_ : Unit) : Int :=
  (Int.sub 5 (-4))

def extern_tdiv (_ : Unit) : Int :=
  (Int.tdiv 5 4)

def extern_tmod (_ : Unit) : Int :=
  (Int.tmod 5 4)

def extern_tmod_positive (_ : Unit) : Int :=
  (Int.tmod 5 4)

def extern_negate (_ : Unit) : Int :=
  (Int.neg (-5))

def extern_mult (_ : Unit) : Int :=
  (Int.mul 5 (-4))

def extern_and (_ : Unit) : Bool :=
  (Bool.and true false)

def extern_and_no_flow (_ : Unit) : Bool :=
  (Bool.and true false)

def extern_or (_ : Unit) : Bool :=
  (Bool.or true false)

def extern_eq_bool (_ : Unit) : Bool :=
  (Eq true false)

def extern_eq_bit (_ : Unit) : Bool :=
  (Eq 0#1 1#1)

def initialize_registers (_ : Unit) : Unit :=
  ()

