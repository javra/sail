def extern_const : BitVec 64 :=
  (0xFFFF000012340000 : BitVec 64)

def extern_add : BitVec 16 :=
  (Add.add (0xFFFF : BitVec 16) (0x1234 : BitVec 16))

def initialize_registers : Unit :=
  ()

