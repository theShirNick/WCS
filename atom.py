from truth_constant import * 
class Atom:
    '''
    A named ground atom
    '''

    def __init__(self, name: str, ground_value: TruthConstant = TruthConstant.UNKNOWN):
        self.ground_value = ground_value
        self.name = name

    def __str__(self):
        return self.name
    def __repr__(self):
        return self.name + "_" + self.ground_value.value

    # def __str__(self):
    #     return self.name + " ⊨ " + self.ground_value.value

    # def __str__(self) -> str:
    #     if self.ground_value == TruthConstant.TRUE:
    #         return self.name+"⁺"
    #     elif self.ground_value == TruthConstant.FALSE:
    #         return self.name+"⁻"
    #     elif self.ground_value == TruthConstant.UNKNOWN:
    #         return self.name+"ⁿ"