import Out.Sail.Sail

open Sail

abbrev SailM := PreSailM (fun (x : PEmpty.{1}) => nomatch x)

def foo (lit : Unit) : Bool :=
  true

def initialize_registers (lit : Unit) : Unit :=
  ()

