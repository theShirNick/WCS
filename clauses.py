from atom import Atom

class Fact:

    def __init__(self, atom: Atom):
        '''
        T → atom 
        '''

        self.atom = atom

class Assumption:

    def __init__(self, atom: Atom):
        '''
        F → atom 
        '''

        self.atom = atom

class Rule:

    def __init__(self, antecedent: Atom, consequent: Atom):
        '''
        antecedent → consequent 

        if antecedent then consequent
        '''

        self.antecedent = antecedent