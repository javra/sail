default Order dec

$include <prelude.sail>

function foo() -> bits(16) = {
  let z = 0xFFFF | 0xABCD in
  0x0000 & z
}

