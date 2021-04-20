from atom import *
from truth_constant import TruthConstant
from program import Program

class Interpretation:
    '''
    An interpretation is three sets of atoms: mapped to T, ⊥, and U, accordingly   
    '''

    def __init__(self, trues: set[Atom], falses: set[Atom], unknowns: set[Atom]):

        self.trues = trues
        self.falses = falses
        self.unknowns = unknowns

        for atom in trues:
            atom.value = TruthConstant.TRUE

        for atom in falses:
            atom.value = TruthConstant.FALSE

        for atom in unknowns:
            atom.value = TruthConstant.UNKNOWN

        if self.trues & self.falses & self.unknowns != set():
            raise Exception("Invalid sets of interpetation. Intersection of trues, falses and unknowns is not equal to empty set")
    
    def __str__(self) -> str:
        s = "〈{"
        for atom in self.trues:
            s = s + atom.name +","
        if self.trues != set(): # if not empty
            s = s[:-1] # delete the last coma
        else:
            s = s + "∅"
        s = s + "}, {"
        for atom in self.falses:
            s = s + atom.name +","
        if self.falses != set(): # if not empty
            s = s[:-1] # delete the last coma
        else:
            s = s + "∅"
        s = s + "}〉"
        return s

    def isModel(self, program: Program) -> bool:
        self.__reset_values()

        isModel = True
        for clause in program.clauses:
            if clause.resolve().value != TruthConstant.TRUE:
                isModel = False
        return isModel

    def __reset_values(self):
        for atom in self.trues:
            atom.value = TruthConstant.TRUE

        for atom in self.falses:
            atom.value = TruthConstant.FALSE

        for atom in self.unknowns:
            atom.value = TruthConstant.UNKNOWN
