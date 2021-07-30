# from infix.expression import InfixExpression
# from infix.tokens import TokenType
# from program import Program
# import itertools
# from clauses import Rule


# def ground(program:Program):
#     with_ground_clauses = program.copy()
#     variables = set()
#     consts = set()
#     for clause in program.clauses:
#             for token in clause.left_head.get_lexer_tokens():
#                 if token.type == TokenType.VAR:
#                     variables.add(token.value)
#                 elif token.type in [TokenType.ATOM, TokenType.CONST]:
#                     consts.add(token.value) 
#             for token in clause.right_body.get_lexer_tokens():
#                 if token.type == TokenType.VAR:
#                     variables.add(token.value)
#                 elif token.type in [TokenType.ATOM, TokenType.CONST]:
#                     consts.add(token.value)

#     substitutions = itertools.product(consts, repeat = len(variables))
#     for sub in substitutions:
#         for clause in program.clauses:
#             new_left_head = clause.left_head.copy()
#             new_right_body = clause.right_body.copy()
#             for i in range(len(variables)):
#                 new_left_head = InfixExpression(new_left_head.replace_var(variables[i], sub[i]), new_left_head.ground_terms)
#                 new_right_body = InfixExpression(new_right_body.replace_var(variables[i], sub[i]))
#             if new_left_head != clause.left_head or new_right_body != clause.right_body:
#                 with_ground_clauses.clauses.append(Rule(new_left_head, new_right_body, clause.non_nec, clause.factual))
#     ground_program_clauses = list()
#     for clause in with_ground_clauses.clauses:
#         if clause.is_ground():
#             ground_program_clauses.append(clause) 

#     return Program(list(set(ground_program_clauses)))