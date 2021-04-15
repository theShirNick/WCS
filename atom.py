from truth_constant import * 
class Atom:
    def __init__(self, value: TruthConstant = TruthConstant.UNKNOWN, name: str = "unnamed_atom" ):
        self.value = value
        self.name = name
    def __str__(self):
        return self.name + " ⊨ " + self.value.value
