from atom import Atom
from phi import phi
from interpretation import Interpretation
from program import Program
from clauses import WC_Rule
from infix.tokens import TokenType
from infix.expression import InfixExpression

class AbducibleFactory:
    '''
    An abducible is a fact A ← ⊤ or A ← ⊥ for an undefined A 
    '''

    def __init__(self, atom: Atom, atom_dict: dict[str, Atom]):
        '''
        atom ← ⊤/⊥

        An abducible is a fact A ← ⊤ or A ← ⊥ for an undefined A 
        '''

        self.atom = atom
        self.atom_dict = atom_dict
    
    def __repr__(self):
        return f"{str(self.atom)} ← ⊤/⊥"

    def __eq__(self, other):
        if self.atom == other.atom:
            return True
        else:
            return False
    
    def __hash__(self):
        return hash(self.atom + 1)

    def get_wc_fact(self) -> WC_Rule:
        '''
        Get Weak Completion Clause atom ← ⊤/⊥ 
        '''

        return WC_Rule(InfixExpression(str(self.atom), self.atom_dict), InfixExpression('T', self.atom_dict))

    def get_wc_assumption(self) -> WC_Rule:
        '''
        Get Weak Completion Clause atom ← ⊥ 
        '''

        return WC_Rule(InfixExpression(str(self.atom), self.atom_dict), InfixExpression('F', self.atom_dict))

class Observation:
    '''
    An observation is an atom or its negation that is not entailed from Phi and should be explained by abduction
    '''

    def __init__(self, infix_expression:InfixExpression):
        '''
        An observation is an atom or its negation that is not entailed from Phi and should be explained by abduction
        '''
        if len(infix_expression.atoms_here) == 1:
            self.infix_expression = infix_expression
        self.atom = infix_expression.atoms_here.pop()
        infix_expression.atoms_here.add(self.atom)

        self.is_negated = False
        count_negs = 0
        for token in infix_expression.get_lexer_tokens():
            if token.type == TokenType.NEGATION:
                count_negs = count_negs + 1
            else:
                break
        if count_negs % 2 == 1:
            self.is_negated = True
        


    def __repr__(self):
            return self.infix_expression.token_string


    def __eq__(self, other):
        if self.infix_expression == other.infix_expression:
            return True
        else:
            return False


    def __hash__(self):
            return hash(self.infix_expression) + 2

def explain_with_abduction(atoms: dict[str, Atom],  wc_program: Program, observations: set[Observation], fixed_point: Interpretation) -> set[Interpretation]:
    result_interpretations = []
    fixed_point_clone = fixed_point.clone()

    # Get undefined atoms as those that do not occur in the body of any clause
    undefined = set(atoms.values())
    for clause in wc_program.clauses:
        if len(clause.body.atoms_here) == 1:
            body_atom = clause.body.atoms_here.pop()
            clause.body.atoms_here.add(body_atom)
            undefined.remove(body_atom)
        else:
            raise Exception(f"Expected the body to have 1 atom; found {len(clause.body.atoms_here)}")

    # Get undefined atoms from fixed point expect the atoms from observations
    # undefined = set(fixed_point_clone.unknowns)
    # for obs in observations:
    #     if obs.atom in undefined:
    #         undefined.remove(obs.atom)
            
    # Generate the set of explanations
    explanations = []
    for u_atom in undefined:
        af = AbducibleFactory(u_atom, atoms)
        assumption = af.get_wc_assumption()
        fact = af.get_wc_fact()
        for explanation in explanations:
            if fact in explanation or assumption in explanation:
                pass
            else:
                new_explanation_fact = explanation.copy()
                new_explanation_fact.add(fact)
                new_explanation_assumption = explanation.copy()
                new_explanation_assumption.add(assumption)
                explanations.append(new_explanation_fact)
                explanations.append(new_explanation_assumption)      
        explanations.append({assumption})
        explanations.append({fact})
        
    
    # run phi with abducible:
    for explanation in explanations:
        prog = wc_program.copy()
        for abducible in explanation: 
            prog.clauses.append(abducible)

        abducted_interpretation = phi(prog, fixed_point_clone)
        next_phi = phi(prog, abducted_interpretation)
        while abducted_interpretation != next_phi:
            abducted_interpretation = next_phi.clone()
            next_phi = phi(prog, next_phi)

        # for safety, make sure that the interpretation is a model
        if not abducted_interpretation.isModel(wc_program):
            raise Exception(f"Interpretation {str(abducted_interpretation)} is not a model")

        # Check if the abducted interpretation explains the observations
        explains_all = True
        for o in observations:
            if o.is_negated:
                if o.atom not in abducted_interpretation.falses:
                    explains_all = False
                    break
            elif not o.is_negated:
                if o.atom not in abducted_interpretation.trues:
                    explains_all = False
                    break

        if explains_all:
            result_interpretations.append(abducted_interpretation)
            for interpretation in result_interpretations:
                if abducted_interpretation.isSuperset(interpretation):
                    result_interpretations.remove(abducted_interpretation)
                    break
                if abducted_interpretation.isSubset(interpretation):
                    result_interpretations.remove(interpretation)
                    
        
    return  result_interpretations

def credulous(interpretations:list[Interpretation]) :
    trues = set()
    falses = set()
    for interpretation in interpretations:
        trues = trues.union(interpretation.trues)
        falses = falses.union(interpretation.falses)
    trues_str = ''
    for true_atom in trues:
        trues_str += f"{true_atom}, "
    if len(trues) > 0:
        trues_str = trues_str[:-2]

    falses_str = ''
    for false_atom in falses:
        falses_str += f"¬{false_atom}, "
    if len(falses) > 0:
        falses_str = falses_str[:-2]

    return f"Credulously follows:\n{trues_str} {falses_str}"

def skeptical(interpretations:list[Interpretation]) :
    trues = interpretations[0].trues.copy()
    falses = interpretations[0].falses.copy()
    for interpretation in interpretations:
        trues = trues.intersection(interpretation.trues)
        falses = falses.intersection(interpretation.falses)
    trues_str = ''
    for true_atom in trues:
        trues_str += f"{true_atom}, "
    if len(trues) > 0:
        trues_str = trues_str[:-2]

    falses_str = ''
    for false_atom in falses:
        falses_str += f"¬{false_atom}, "
    if len(falses) > 0:
        falses_str = falses_str[:-2]
    return f"Skeptically follows:\n{trues_str} {falses_str}"

