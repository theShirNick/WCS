from atom import *
from truth_tables import * 
import unittest
import random

T = Atom(TruthConstant.TRUE, "T")
F = Atom(TruthConstant.FALSE, "F")
U = Atom(TruthConstant.UNKNOWN, "U")
def getRandomAtom():
    rnd = random.randint(0, 2)
    if rnd == 0:
        return F
    elif rnd == 1:
        return T
    elif rnd == 2:
        return U


class TestTruthTables(unittest.TestCase):
    def test_negation(self):
        self.assertEqual(negation(T).value, TruthConstant.FALSE)
        self.assertEqual(negation(F).value, TruthConstant.TRUE)
        self.assertEqual(negation(U).value, TruthConstant.UNKNOWN)

    def test_conjunction(self):
        self.assertEqual(conjunction(T,T).value, TruthConstant.TRUE)
        self.assertEqual(conjunction(U,U).value, TruthConstant.UNKNOWN)
        self.assertEqual(conjunction(U,T).value, TruthConstant.UNKNOWN)
        self.assertEqual(conjunction(getRandomAtom(),F).value, TruthConstant.FALSE)
        self.assertEqual(conjunction(F,getRandomAtom()).value, TruthConstant.FALSE)
    
    def test_disjunction(self):
        self.assertEqual(disjunction(T,getRandomAtom()).value, TruthConstant.TRUE)
        self.assertEqual(disjunction(getRandomAtom(),T).value, TruthConstant.TRUE)
        self.assertEqual(disjunction(U,U).value, TruthConstant.UNKNOWN)
        self.assertEqual(disjunction(F,U).value, TruthConstant.UNKNOWN)   

    def test_implication(self):
        self.assertEqual(implication(F,getRandomAtom()).value, TruthConstant.TRUE) 
        self.assertEqual(implication(getRandomAtom(),T).value, TruthConstant.TRUE) 
        self.assertNotEqual(implication(U,getRandomAtom()).value, TruthConstant.FALSE) 
        self.assertEqual(implication(U,U).value, TruthConstant.TRUE)
    
    def test_equivalence(self):
        self.assertEqual(equivalence(F,F).value, TruthConstant.TRUE)
        self.assertEqual(equivalence(U,U).value, TruthConstant.TRUE)
        self.assertEqual(equivalence(T,F).value, TruthConstant.FALSE) 
        self.assertEqual(equivalence(U,T).value, TruthConstant.UNKNOWN) 

    def test_combined(self):
        self.assertEqual(equivalence(negation(T), conjunction(F, getRandomAtom())).value, TruthConstant.TRUE)
        self.assertEqual(implication(negation(disjunction(T, getRandomAtom())), getRandomAtom()).value, TruthConstant.TRUE)

    def test_str(self):
        R = getRandomAtom()
        self.assertEqual(str(equivalence(negation(T), conjunction(F, R))), f"¬T↔F∧{R.name} ⊨ T")
        R1 = getRandomAtom()
        R2 = getRandomAtom()
        self.assertEqual(str(implication(negation(disjunction(T, R1)), R2)), f"¬(T∨{R1.name})→{R2.name} ⊨ T")
        self.assertEqual(str(disjunction(F,negation(U))), f"F∨¬U ⊨ U")
  