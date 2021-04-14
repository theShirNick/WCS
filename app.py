from atom import Atom
import truth_tables as tt
import truth_constant as tc

a = Atom(tc.TruthConstant.UNKNOWN)
b = Atom(tc.TruthConstant.UNKNOWN)
c = Atom(tc.TruthConstant.TRUE)
res = tt.implication(a,tt.conjunction(b,c))
print(res.value)