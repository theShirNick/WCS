from truth_constant import * 
class Atom:
    '''
    A named ground atom
    '''

    def __init__(self, name: str, value: TruthConstant = TruthConstant.UNKNOWN):
        self.value = value
        self.name = name

    def __str__(self):
        return self.name + "_" + self.value.value

    # def __str__(self):
    #     return self.name + " ⊨ " + self.value.value

    # def __str__(self) -> str:
    #     if self.value == TruthConstant.TRUE:
    #         return self.name+"⁺"
    #     elif self.value == TruthConstant.FALSE:
    #         return self.name+"⁻"
    #     elif self.value == TruthConstant.UNKNOWN:
    #         return self.name+"ⁿ"