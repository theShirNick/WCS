from truth_constant import TruthConstant
from atom import Atom
from phi import phi
from interpretation import Interpretation
from program import Program
from clauses import Rule, WC_Rule
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
        Get Weak Completion Clause from 'atom ← ⊤' 
        '''

        return WC_Rule(InfixExpression(str(self.atom), self.atom_dict), InfixExpression('T', self.atom_dict))

    def get_wc_assumption(self) -> WC_Rule:
        '''
        Get Weak Completion Clause from 'atom ← ⊥' 
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
def get_undefined_atoms(atoms: dict[str, Atom],  wc_program: Program):
    '''
        Get undefined atoms as those that do not occur in the head of any clause
    '''

    undefined = set(atoms.values())
    for clause in wc_program.clauses:
        if len(clause.left_head.atoms_here) == 1:
            head_atom = clause.left_head.atoms_here.pop()
            clause.left_head.atoms_here.add(head_atom)
            undefined.remove(head_atom)
        else:
            raise Exception(f"Expected the head to have 1 atom; found {len(clause.left_head.atoms_here)}")
    return undefined
            
def get_set_of_abducibles(atoms: dict[str, Atom],  wc_program: Program):
    
    undefined = get_undefined_atoms(atoms, wc_program)
    # Generate the set of explanations

    # Classification of conditionals alters the set of abducibles
    classification_enforced = set()
    for clause in wc_program.clauses:
        if clause.non_nec:
            classification_enforced.add(WC_Rule(clause.left_head, InfixExpression('T', atoms)))
        if clause.factual:
            for atom in clause.right_body.atoms_here:
                if atom.is_abnormality:
                    classification_enforced.add(WC_Rule(InfixExpression(f"{atom.name}", atoms), InfixExpression('T', atoms)))
    
    explanations = []
    for u_atom in undefined: # for each undefined atom there can be two explanations: it's either true or false
        af = AbducibleFactory(u_atom, atoms)
        assumption = af.get_wc_assumption()
        fact = af.get_wc_fact()


        for explanation in explanations:
            if fact in explanation or assumption in explanation:
                pass # we don't need contradictory explanations
            else:
                new_explanation_fact = explanation.copy()
                new_explanation_fact.add(fact)
                new_explanation_assumption = explanation.copy()
                new_explanation_assumption.add(assumption)
                explanations.append(new_explanation_fact) # append existing explanation
                explanations.append(new_explanation_assumption)      
        explanations.append({assumption}) # make a new explanation with just this assumption
        explanations.append({fact}) # make a new explanation with just this fact
        if len(classification_enforced) > 0:
            explanations.append(classification_enforced) # extend the set by classification additions
    return explanations 

def phi_with_abduction(explanations: list, wc_program: Program,  observations: set[Observation], fixed_point: Interpretation, integrity_constraints: set[Rule]):
    result_interpretations = []
    fixed_point_clone = fixed_point.clone()
    for explanation in explanations:
        prog = wc_program.copy()
        for abducible in explanation: 
            prog.clauses.append(abducible)

        abduced_interpretation = phi(prog, fixed_point_clone)
        next_phi = phi(prog, abduced_interpretation)
        while abduced_interpretation != next_phi:
            abduced_interpretation = next_phi.clone()
            next_phi = phi(prog, next_phi)

        # Check if abduced interpretation satisfies integrity constraints
        integrity_constraint_check = True
        for constraint in integrity_constraints:
            if constraint.evaluate() != TruthConstant.TRUE:
                integrity_constraint_check = False
                break
        if not integrity_constraint_check:
            continue

        # Check if the abduced interpretation explains the observations
        explains_all = True
        for o in observations:
            if o.is_negated:
                if o.atom not in abduced_interpretation.falses:
                    explains_all = False
                    break
            elif not o.is_negated:
                if o.atom not in abduced_interpretation.trues:
                    explains_all = False
                    break

        if explains_all:
            result_interpretations.append(abduced_interpretation)
            for interpretation in result_interpretations:
                if abduced_interpretation.isSuperset(interpretation):
                    result_interpretations.remove(abduced_interpretation)
                    break
                if abduced_interpretation.isSubset(interpretation):
                    result_interpretations.remove(interpretation)
    return  result_interpretations


# def credulous(interpretations:list[Interpretation]) :
#     trues = set()
#     falses = set()
#     for interpretation in interpretations:
#         trues = trues.union(interpretation.trues)
#         falses = falses.union(interpretation.falses)
#     trues_str = ''
#     for true_atom in trues:
#         trues_str += f"{true_atom}, "
#     if len(trues) > 0:
#         trues_str = trues_str[:-2]

#     falses_str = ''
#     for false_atom in falses:
#         falses_str += f"¬{false_atom}, "
#     if len(falses) > 0:
#         falses_str = falses_str[:-2]

#     return f"Credulously follows:\n{trues_str} {falses_str}"

def skeptical(atoms: dict[str, Atom],  wc_program: Program, interpretations:list[Interpretation]) :
    undefined = get_undefined_atoms(atoms, wc_program)
    trues = interpretations[0].trues.copy()
    falses = interpretations[0].falses.copy()
    for interpretation in interpretations:
        trues = trues.intersection(interpretation.trues).intersection(undefined)
        falses = falses.intersection(interpretation.falses).intersection(undefined)
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
    return_str = 'Skeptically follows:\n'
    if len(trues) > 0:
        return_str = return_str + f"{trues_str}, "
    if len(falses) > 0:
        return_str = return_str + f"{falses_str} "
    if len(trues) == 0 and len(falses) == 0:
        return_str = return_str + f"nothing new"
    return return_str

