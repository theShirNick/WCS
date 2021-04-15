from atom import Atom
import truth_tables as tt
import truth_constant as tc

a = Atom(tc.TruthConstant.UNKNOWN, "a")
b = Atom(tc.TruthConstant.UNKNOWN, "b")
c = Atom(tc.TruthConstant.TRUE, "c")
res = tt.implication(a,tt.conjunction(b,c))
print(res)
print(str(tt.disjunction(a,tt.negation(tt.conjunction(b,c)))))