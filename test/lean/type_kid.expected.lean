import Out.Sail.Sail

open Sail

abbrev SailM := PreSailM (fun (x : PEmpty.{1}) => nomatch x)

/-- Type quantifiers: k_a : Type -/
def foo (x : k_a) : (k_a × k_a) :=
  (x, x)

def initialize_registers (lit : Unit) : Unit :=
  ()

