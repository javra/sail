import Out.Sail.Sail

open Sail

inductive E where | A | B | C
  deriving Inhabited

def undefined_E : SailM E := do
  (pure sorry)

def initialize_registers : Unit :=
  ()

