from atom import *
from truth_tables import * 

a = Atom(TruthConstant.FALSE, "a")
b = Atom(TruthConstant.UNKNOWN, "b")
c = Atom(TruthConstant.TRUE, "c")
print(f"with a = {a.value.value}, with b = {b.value.value}, with c = {c.value.value}")
res = implication(a,conjunction(b,c))
print(res)
print(str(disjunction(a,negation(conjunction(b,c)))))
print(str(disjunction(a,conjunction(disjunction(b,c), a))))
