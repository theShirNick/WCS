from truth_constant import * 
class Atom:
    '''
    A named ground atom
    '''

    def __init__(self, name: str, ground_value: TruthConstant = TruthConstant.UNKNOWN, is_abnormality = False):
        self.ground_value = ground_value
        self.name = name
        self.is_abnormality = is_abnormality

    def __str__(self):
        if self.is_abnormality:
            return self.name.replace('ab_', '*ab*')
        else:
            return self.name
    def __repr__(self):
        repr_str = ''
        if self.is_abnormality == True:
            repr_str = repr_str + 'ᵃᵇ'
        repr_str = repr_str + self.name + "_" + self.ground_value.value
        return repr_str

    # def __str__(self):
    #     return self.name + " ⊨ " + self.ground_value.value

    # def __str__(self) -> str:
    #     if self.ground_value == TruthConstant.TRUE:
    #         return self.name+"⁺"
    #     elif self.ground_value == TruthConstant.FALSE:
    #         return self.name+"⁻"
    #     elif self.ground_value == TruthConstant.UNKNOWN:
    #         return self.name+"ⁿ"