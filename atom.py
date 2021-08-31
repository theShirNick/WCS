from truth_constant import * 
class Atom:
    '''
    A named ground atom
    '''

    def __init__(self, predicate: str, arguments:list, is_ground:bool):
        self.predicate = predicate
        self.arguments = arguments
        self.is_ground = is_ground
        self.string = ''
        if len(self.arguments) > 1:
            args = ''
            for arg in self.arguments:
                args = args + arg +','
            self.string = f'<font color="PaleGreen">{self.predicate}</font>({args[:-1]})'
        elif self.predicate != None:
            self.string =  f'<font color="PaleGreen">{self.predicate}</font> {self.arguments[0]}'
        else:
            self.string = self.arguments[0]

    def __str__(self):
       return self.string
    def __repr__(self):
        return self.string

    # def __str__(self):
    #     return self.name + " ⊨ " + self.ground_value.value

    # def __str__(self) -> str:
    #     if self.ground_value == TruthConstant.TRUE:
    #         return self.name+"⁺"
    #     elif self.ground_value == TruthConstant.FALSE:
    #         return self.name+"⁻"
    #     elif self.ground_value == TruthConstant.UNKNOWN:
    #         return self.name+"ⁿ"