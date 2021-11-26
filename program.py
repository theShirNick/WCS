from clauses import *


class Program:
    '''
    A program is a conjunction of all clauses 
    '''

    def __init__(self, clauses: list[Clause]):

        self.clauses = clauses

    def __str__(self) -> str:
        if len(self.clauses) == 0:
            return ''
        s = ""
        for clause in self.clauses:
            s = s + str(clause)  +";<br>" 
        return s[:-5]
    
    def latex(self) -> str:
        if len(self.clauses) == 0:
            return ''
        s = ""
        for clause in self.clauses:
            s = s + clause.latex() 
        return s[:-7]+'.'

    def __eq__(self, o: object) -> bool:
        if type(o) != type(self):
            return False
        if len(self.clauses) != len(o.clauses):
            return False
        for clause in self.clauses:
            if clause not in o.clauses:
                return False
        return True
    
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
                    head_body[str(clause.left_head)] = [clause.left_head, clause.right_body]
                else:
                    head_body[str(clause.left_head)] = [clause.left_head, head_body[str(clause.left_head)][1].disjoin(clause.right_body)]
        for key in head_body:
            wc_program.clauses.append(WC_Rule(head_body[key][0], head_body[key][1]))     
        return wc_program

            


    

    