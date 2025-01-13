import Out.Sail.Sail

open Sail

def foo (n : Int) : BitVec 4 :=
  (0xF : BitVec 4)

def bar (x : BitVec k_n) : BitVec k_n :=
  x

def initialize_registers : Unit :=
  ()

