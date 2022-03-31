from truth_constant import * 
from CONSTANTS import *
class Atom:
    '''
    A named ground term
    '''

    def __init__(self, predicate: str, arguments:list, is_ground:bool):
        self.predicate = predicate
        self.arguments = arguments
        self.is_ground = is_ground
        self.string = ''
        if len(self.arguments) > 1:
            args = ''
            for arg in self.arguments:
                args = args + f'{ARGUMENT_FONT}{arg}</font>' +','
            self.string = f'{PREDICATE_FONT}{self.predicate}</font>({args[:-1]})'
        elif self.predicate != None:
            self.string =  f'{PREDICATE_FONT}{self.predicate}</font> {ARGUMENT_FONT}{self.arguments[0]}</font>'
        else:
            self.string = f'{ARGUMENT_FONT}{self.arguments[0]}</font>'

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