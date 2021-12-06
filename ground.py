import itertools
from infix.expression import InfixExpression
from infix.tokens import TokenType
from clauses import Rule



def find_vars_and_consts(clauses, variables, consts):
    for clause in clauses:
            for token in clause.left_head.get_lexer_tokens():
                if token.type == TokenType.VAR:
                    if token.value not in variables:
                        variables.append(token.value)
                elif token.type in [TokenType.ATOM, TokenType.CONST]:
                    if token.value not in consts:
                        consts.append(token.value) 
            for token in clause.right_body.get_lexer_tokens():
                if token.type == TokenType.VAR:
                    if token.value not in variables:
                        variables.append(token.value)
                elif token.type in [TokenType.ATOM, TokenType.CONST]:
                    if token.value not in consts:
                        consts.append(token.value)


def ground(clauses, variables, consts):
    ground_clauses = set()
    non_ground_clauses = set()

    for clause in clauses:
        if clause.is_ground():
            ground_clauses.add(clause)
        else:
            non_ground_clauses.add(clause)

    substitutions = list(itertools.product(consts, repeat = len(variables)))
    for sub in substitutions:
        for clause in non_ground_clauses:
            
            new_left_head = clause.left_head
            new_right_body = clause.right_body
            new_left_head = InfixExpression(new_left_head.replace_var(variables, sub), new_left_head.ground_terms)
            new_right_body = InfixExpression(new_right_body.replace_var(variables, sub), new_right_body.ground_terms)
            ground_clauses.add(Rule(new_left_head, new_right_body, clause.non_nec, clause.factual))

    
    return list(ground_clauses)
    