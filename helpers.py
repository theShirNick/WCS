import sys
import os
from infix.tokens import TokenType 

def resource_path(relative_path):
        """ Get absolute path to resource.
        Switch for running from source or for frozen PyInstaller instance."""
        try:
            # PyInstaller creates a temp folder and stores path in _MEIPASS
            base_path = sys._MEIPASS
        except Exception:
            base_path = os.path.abspath(".")
        returnMe = os.path.join(base_path, relative_path)
        return returnMe

def get_predicates(clauses) -> set[str]:
    predicates = set()

    '''
    Populate a set of predicates based on token type
    '''
    for clause in clauses:
        for token in clause.left_head.get_lexer_tokens():
            if token.type == TokenType.PREDICATE:
                if token.value not in predicates:
                    predicates.add(token.value)
        for token in clause.right_body.get_lexer_tokens():
           if token.type == TokenType.PREDICATE:
                if token.value not in predicates:
                    predicates.add(token.value)
    return predicates