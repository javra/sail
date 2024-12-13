default Order dec

$include <prelude.sail>

val foo : forall 'n. int('n) -> bits(4)

function foo(n) = {
  0xF
}

val bar : forall 'n. bits('n) -> bits('n)

function bar(x) = {
  x
}
