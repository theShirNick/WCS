from atom import *
from truth_constant import TruthConstant
from program import Program

class Interpretation:
    '''
    An interpretation is three sets of atoms: mapped to ⊤, ⊥, and U, accordingly   
    '''

    def __init__(self, trues: set[Atom], falses: set[Atom], unknowns: set[Atom]):

        self.trues = trues
        self.falses = falses
        self.unknowns = unknowns

        for atom in trues:
            atom.ground_value = TruthConstant.TRUE

        for atom in falses:
            atom.ground_value = TruthConstant.FALSE

        for atom in unknowns:
            atom.ground_value = TruthConstant.UNKNOWN

        if self.trues & self.falses != set() or self.falses & self.unknowns != set() or self.trues & self.unknowns != set():
            raise Exception("Invalid sets of interpetation. Sets must be mutually exclusive")
    
    def __str__(self) -> str:
        s = "〈{"
        for atom in self.trues:
            s = s + atom.name +", "
        if self.trues != set(): # if not empty
            s = s[:-2] # delete the last comma
        else:
            s = s + "∅"
        s = s + "}, {"
        for atom in self.falses:
            s = s + atom.name +", "
        if self.falses != set(): # if not empty
            s = s[:-2] # delete the last comma 
        else:
            s = s + "∅"
        s = s + "}〉"
        return s

    def isModel(self, program: Program) -> bool:
        self.__reset_values()

        isModel = True
        for clause in program.clauses:
            if clause.evaluate() != TruthConstant.TRUE:
                isModel = False
        return isModel

    def __reset_values(self):
        for atom in self.trues:
            atom.ground_value = TruthConstant.TRUE

        for atom in self.falses:
            atom.ground_value = TruthConstant.FALSE

        for atom in self.unknowns:
            atom.ground_value = TruthConstant.UNKNOWN
    
    def clone(self) -> 'Interpretation':
        return Interpretation(self.trues.copy(), self.falses.copy(), self.unknowns.copy())
    
    def __eq__(self, other):
        if self.trues == other.trues and self.falses == other.falses and self.unknowns == other.unknowns:
            return True
        else:
            return False
