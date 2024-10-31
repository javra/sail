default Order dec

function tuple1() -> (int, int, (bitvector(2), unit)) = {
  return (3, 5, (0b10, ()))
}

