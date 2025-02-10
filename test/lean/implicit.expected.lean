import Out.Sail.Sail

open Sail

abbrev SailM := PreSailM (fun (x : PEmpty.{1}) => nomatch x)

/-- Type quantifiers: k_n : Int, m : Int, m ≥ k_n -/
def EXTZ {m : _} (v : (BitVec k_n)) : (BitVec m) :=
  (Sail.BitVec.zeroExtend v m)

def foo (x : (BitVec 8)) : (BitVec 16) :=
  (EXTZ x)

def initialize_registers (lit : Unit) : Unit :=
  ()

