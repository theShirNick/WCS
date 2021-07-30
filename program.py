from clauses import *

from infix.tokens import TokenType
import itertools

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
        return s[:-1]
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


    def ground(self):
        with_ground_clauses = self.copy()
        variables = set()
        consts = set()
        for clause in self.clauses:
                for token in clause.left_head.get_lexer_tokens():
                    if token.type == TokenType.VAR:
                        variables.add(token.value)
                    elif token.type in [TokenType.ATOM, TokenType.CONST]:
                        consts.add(token.value) 
                for token in clause.right_body.get_lexer_tokens():
                    if token.type == TokenType.VAR:
                        variables.add(token.value)
                    elif token.type in [TokenType.ATOM, TokenType.CONST]:
                        consts.add(token.value)
        variables = list(variables)
        consts = list(consts)
        substitutions = itertools.product(consts, repeat = len(variables))
        for sub in substitutions:
            for clause in self.clauses:
                new_left_head = clause.left_head
                new_right_body = clause.right_body
                for i in range(len(variables)):
                    new_left_head = InfixExpression(new_left_head.replace_var(variables[i], sub[i]), new_left_head.ground_terms)
                    new_right_body = InfixExpression(new_right_body.replace_var(variables[i], sub[i]), new_right_body.ground_terms)
                if new_left_head != clause.left_head or new_right_body != clause.right_body:
                    with_ground_clauses.clauses.append(Rule(new_left_head, new_right_body, clause.non_nec, clause.factual))
        ground_program_clauses = list()
        for clause in with_ground_clauses.clauses:
            if clause.is_ground():
                ground_program_clauses.append(clause) 

        ground_program_clauses = list(dict.fromkeys(ground_program_clauses))
        
        return Program(ground_program_clauses)

            


    

    