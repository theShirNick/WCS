from atom import *
from truth_tables import * 
from clauses import *
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


class TestClauses(unittest.TestCase):
    def test_str(self):
        R = getRandomAtom()
        self.assertEqual(str(Fact(R)), f"{R.name} ← {TruthConstant.TRUE.value}")
        R = getRandomAtom()
        self.assertEqual(str(Assumption(R)), f"{R.name} ← {TruthConstant.FALSE.value}")
        R1 = getRandomAtom()
        R2 = getRandomAtom()
        self.assertEqual(str(Rule(R1,R2)), f"{R1.name} ← {R2.name}")
        