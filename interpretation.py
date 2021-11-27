from atom import *
from truth_constant import TruthConstant
from program import Program

class Interpretation:
    '''
    An interpretation is three sets of atoms: mapped to ⊤, ⊥, and U, accordingly   
    '''

    def __init__(self, ground_terms:dict[str, TruthConstant], trues:set[str], falses:set[str], unknowns:set[str]):
        self.ground_terms = ground_terms
        self.trues = trues
        self.falses = falses
        self.unknowns = unknowns

        for true_term in trues:
            ground_terms[true_term] = TruthConstant.TRUE

        for false_term in falses:
            ground_terms[false_term] = TruthConstant.FALSE

        for unknown_term in unknowns:
            self.ground_terms[unknown_term] = TruthConstant.UNKNOWN

        if self.trues & self.falses != set() or self.falses & self.unknowns != set() or self.trues & self.unknowns != set():
            raise Exception("Invalid sets of interpetation. Sets must be mutually exclusive")
    
    def __repr__(self) -> str:
        s = "〈{"
        for true_term in self.trues:
            s = s + true_term +", "
        if self.trues != set(): # if not empty
            s = s[:-2] # delete the last comma
        else:
            s = s + "∅"
        s = s + "}, {"
        for false_term in self.falses:
            s = s + false_term+", "
        if self.falses != set(): # if not empty
            s = s[:-2] # delete the last comma 
        else:
            s = s + "∅"
        s = s + "}〉"
        return s
    def latex(self):
        s = ''
        for true_term in self.trues:
            s = s + f"{str(true_term)}, "
        if len(self.trues) > 0:
            s = s[:-2]
        s = s + ' & '
        for false_term in self.falses:
            s = s + f"{str(false_term)}, "
        if len(self.falses) > 0:
            s = s[:-2]
        return s

    def isModel(self, program: Program) -> bool:
        self.__reset_values()

        isModel = True
        for clause in program.clauses:
            if clause.evaluate() != TruthConstant.TRUE:
                isModel = False
        return isModel

    def isSubset(self, other:'Interpretation') -> bool:
        if self == other:
            return False
        if self.falses.issubset(other.falses):
            if self.trues.issubset(other.trues):
                return True
        return False
    
    def isSuperset(self, other:'Interpretation') -> bool:
        if self == other:
            return False
        if self.falses.issuperset(other.falses):
            if self.trues.issuperset(other.trues):
                return True
        return False

    def __reset_values(self):
        for true_term in self.trues:
            self.ground_terms[true_term] = TruthConstant.TRUE

        for false_term in self.falses:
            self.ground_terms[false_term] = TruthConstant.FALSE

        for unknown_term in self.unknowns:
            self.ground_terms[unknown_term] = TruthConstant.UNKNOWN
    
    def clone(self) -> 'Interpretation':
        return Interpretation(self.ground_terms, self.trues.copy(), self.falses.copy(), self.unknowns.copy())
    
    def __eq__(self, other):
        # if self == None or other == None:
        #     return False
        if self.trues == other.trues and self.falses == other.falses and self.unknowns == other.unknowns:
            return True
        else:
            return False
    
    
