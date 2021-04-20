from clauses import *

class Program:
    '''
    A program is a conjunction of all clauses 
    '''

    def __init__(self, clauses: list[Clause]):

        self.clauses = clauses

    def __str__(self) -> str:
        s = ""
        for clause in self.clauses:
            s = s + str(clause)  +",\n" 
        return s[:-2]
    
    

    