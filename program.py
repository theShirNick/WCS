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
            s = s + str(clause)  +";\n" 
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
        head_body = dict()

        for clause in self.clauses:
                if str(clause.left_head) not in head_body:
                    head_body[str(clause.left_head)] = [clause.left_head, clause.right_body, clause.non_nec, clause.factual]
                else:
                    # TODO what should I do if classifications don't match? (e.g nn True + False)
                    head_body[str(clause.left_head)] = [clause.left_head, head_body[str(clause.left_head)][1].disjoin(clause.right_body), clause.non_nec, clause.factual]
        for key in head_body:
            wc_program.clauses.append(WC_Rule(head_body[key][0], head_body[key][1], head_body[key][2], head_body[key][3] ))     
        return wc_program



            


    

    