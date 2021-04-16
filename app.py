from program import Program
from atom import *
from truth_tables import *
from clauses import *

a = Atom(TruthConstant.FALSE, "a")
fact = Fact(a)

b = Atom(TruthConstant.TRUE, "b")
assump = Assumption(b)

rule = Rule(a,b)

prog = Program([fact, assump, rule])

print(str(prog))


