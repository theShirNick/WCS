from atom import *
from truth_tables import * 
from clauses import *
from program import *
import unittest
import random

T = Atom(TruthConstant.TRUE, "T_Atom")
F = Atom(TruthConstant.FALSE, "F_Atom")
U = Atom(TruthConstant.UNKNOWN, "U_Atom")
def getRandomAtom():
    rnd = random.randint(0, 2)
    if rnd == 0:
        return F
    elif rnd == 1:
        return T
    elif rnd == 2:
        return U

fact = Fact(getRandomAtom())
assump = Assumption(getRandomAtom())
rule = Rule(getRandomAtom(), getRandomAtom())


class TestProgram(unittest.TestCase):
    def test_str(self):
        self.assertEqual(str(Program([fact, assump, rule])), f"{str(fact)},\n{str(assump)},\n{str(rule)}")