default Order dec

$include <prelude.sail>

function unif_bitvec_append(x: bitvector(13), y: bitvector(3)) -> bitvector(4*4) = {
    bitvector_concat(x, y)
}

function unif_bitvec_replicate(x: bitvector(4)) -> bitvector(2*8) = {
    replicate_bits(x, 4)
}

function unif_subrange_bits(x: bitvector(16)) -> bitvector(17 - 10 + 1) = {
    subrange_bits(x, 10, 3)
}

