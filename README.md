# k-bit-multiplexer

Learning classifier system for the k-bit multiplexer problem.

Given a bit string of length k = l + 2^l, where the first part represents an
address and the second part represents data, return the bit of the data part
at the index signified by the address part.

For example, the string 101011 has 2 address bits and 2^2 = 4 data bits, for
a total of k = 6 bits. Since the address bits `10` decode to the value 2,
the solution is the second bit of the data part `1011`, which is 0.

The goal of this project is to evolve a set of rules that correctly generalize
the problem by using the "don't care" symbol `#` to ignore irrelevant
information. In the above example, the only bits we care about are the address
part and the second bit of the data part, which would be represented as
10#0##, so that any string matching this pattern, with any bits taking the
place of the # symbols, has a solution of 0.