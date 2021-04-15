from atom import *
from truth_tables import * 
from clauses import *
import unittest
import random

T = Atom(TruthConstant.TRUE)
F = Atom(TruthConstant.FALSE)
U = Atom(TruthConstant.UNKNOWN)
def getRandomAtom():
    rnd = random.randint(0, 2)
    if rnd == 0:
        return F
    elif rnd == 1:
        return T
    elif rnd == 2:
        return U


class TestClauses(unittest.TestCase):
    def test_negation(self):
        pass