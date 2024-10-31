default Order dec

$include <prelude.sail>

function extern_const() -> bitvector(64) = {
  return 0xFFFF_0000_1234_0000
}

function extern_add() -> bitvector(16) = {
  return 0xFFFF + 0x1234
}

