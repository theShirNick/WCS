from truth_constant import * 
class Atom:
    def __init__(self, name: str, value: TruthConstant = TruthConstant.UNKNOWN):
        self.value = value
        self.name = name
    def __str__(self):
        return self.name + " ⊨ " + self.value.value
