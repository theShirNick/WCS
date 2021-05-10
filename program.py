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
    
    def copy(self):
        copied_clauses = []
        for clause in self.clauses:
            copied_clauses.append(clause)
        return Program(copied_clauses)
    

    def weakly_complete(self) -> 'Program':
        '''
        Replace all occurances of ← by ↔

        Disjoin the heads of clauses with equal bodies
        '''
        wc_program = Program([])
        body_head = dict()

        for clause in self.clauses:
                if str(clause.body) not in body_head:
                    body_head[str(clause.body)] = [clause.body, clause.head]
                else:
                    body_head[str(clause.body)] = [clause.body, body_head[str(clause.body)][1].disjoin(clause.head)]
        for key in body_head:
            wc_program.clauses.append(WC_Rule(body_head[key][0], body_head[key][1]))     
        return wc_program



            


    

    