from infix.tokens import TokenType
from infix.nodes import *

class Parser:
    def  __init__(self, tokens, ground_terms:dict) -> None:
        self.tokens = iter(tokens)
        self.ground_terms = ground_terms
        self.advance()

    def raise_error(self):
        raise Exception("Invalid syntax. Could not perform parsing of tokens")

    def advance(self):
        try:
            self.current_token = next(self.tokens)
        except StopIteration:
            self.current_token = None
        
    def parse(self):
        if self.current_token == None: #if empty input
            return None
        result = self.expr()

        if self.current_token != None:
            self.raise_error()

        return result

    def expr(self):
        '''
        An expression is contained in parentheses or is the entire input. 

            a&b is an expression

            (a|b)&(c|d) is an expression that contains two other expressions

        Order of operations: 
        1. Parentheses 
        2. Negation
        3. Conjunctions
        4. Disjunctions <- you are looking for
        '''

        result = self.term()

        while self.current_token != None and self.current_token.type == TokenType.DISJUNCTION:
            self.advance()
            result = DisjunctionNode(result, self.term())
        
        return result

    def term(self):
        '''
        A term can be part of a disjunction.

        There are two terms in the expression a&~b|c:

            a&~b and c
        
        Order of operations:
        1. Parentheses
        2. Negation
        3. Conjunctions <- you are looking for
        4. Disjunctions 
        '''

        result = self.factor()

        while self.current_token != None and self.current_token.type == TokenType.CONJUNCTION:
            self.advance()
            result = ConjunctionNode(result, self.factor())
        
        return result

    def factor(self):
        '''
        A factor can be part of a conjuntion.

        There are four factors in the expression a&~b|c:

            a, b, ~b and c 

        Order of operations:
        1. Parentheses      <- you are looking for new expressions
        2. Negation         <- you are looking for atoms and their negations
        3. Conjunctions
        4. Disjunctions
        '''

        token = self.current_token

        if token.type == TokenType.ORDER_LPAREN:
            self.advance()
            result = self.expr() # anything in order parantheses is a whole new expression

            if self.current_token.type != TokenType.ORDER_RPAREN:
                self.raise_error()
            
            self.advance()
            return result

        elif token.type == TokenType.ATOM:
            self.advance()
            atom = Atom(None, [token.value], True)
            if atom.string not in self.ground_terms:
                self.ground_terms[atom.string] = TruthConstant.UNKNOWN 
            return AtomNode(atom)
        
        elif token.type == TokenType.VAR:
            self.advance()
            atom = Atom(None, [token.value], False)
            return AtomNode(atom)

        elif self.current_token.type == TokenType.PREDICATE:
            predicate = self.current_token.value
            args = list()
            is_ground = True
            self.advance()
            if self.current_token.type == TokenType.PRED_LPAREN:
                self.advance()
                while self.current_token.type in [TokenType.CONST, TokenType.VAR, TokenType.COMMA]:
                    if self.current_token.type == TokenType.CONST:
                        args.append(self.current_token.value)
                    if self.current_token.type == TokenType.VAR:
                        args.append(self.current_token.value)
                        is_ground = False    
                    self.advance()
                if self.current_token.type == TokenType.PRED_RPAREN:
                    self.advance()
                else:
                    self.raise_error()
            elif self.current_token.type == TokenType.CONST:
                args.append(self.current_token.value)
                self.advance()
            elif self.current_token.type == TokenType.VAR:
                is_ground = False
                args.append(self.current_token.value)
                self.advance()
            atom = Atom(predicate, args, is_ground)
            if atom.is_ground and atom.string not in self.ground_terms:
                self.ground_terms[atom.string] = TruthConstant.UNKNOWN                
            return AtomNode(atom)
        
        elif token.type == TokenType.TRUTHCONST:
            self.advance()
            return TruthConstNode(token.value)

        elif token.type == TokenType.NEGATION:
            self.advance()
            return NegationNode(self.factor())
        
        self.raise_error()