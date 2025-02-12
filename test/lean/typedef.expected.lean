import Out.Sail.Sail

open Sail

abbrev xlen : Int := 64

abbrev xlen_bytes : Int := 8

/- Type abbreviation omitted: xlenbits -/

/- Type abbreviation omitted: bits2 -/

abbrev SailM := PreSailM PEmpty.elim trivialChoiceSource

/-- Type quantifiers: k_n : Int, m : Int, m ≥ k_n -/
def EXTZ {m : _} (v : (BitVec k_n)) : (BitVec m) :=
  (Sail.BitVec.zeroExtend v m)

/-- Type quantifiers: k_n : Int, m : Int, m ≥ k_n -/
def EXTS {m : _} (v : (BitVec k_n)) : (BitVec m) :=
  (Sail.BitVec.signExtend v m)

def initialize_registers (_ : Unit) : Unit :=
  ()

