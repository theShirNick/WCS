from truth_constant import TruthConstant
from atom import Atom
from phi import phi
from interpretation import Interpretation
from program import Program
from clauses import Rule, WC_Rule
from infix.tokens import TokenType
from infix.expression import InfixExpression


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
            return self.infix_expression.node_string


    def __eq__(self, other):
        if self.infix_expression == other.infix_expression:
            return True
        else:
            return False


    def __hash__(self):
            return hash(self.infix_expression) + 2

def get_undefined_atoms(atoms: dict[str, Atom],  program: Program):
    '''
        Get undefined atoms as those that do not occur in the head of any clause
    '''

    undefined = set(atoms.values()) # consider all atoms
    for clause in program.clauses:
        if len(clause.left_head.atoms_here) == 1: # if an atom is in the head of a clause...
            head_atom = clause.left_head.atoms_here.pop()
            clause.left_head.atoms_here.add(head_atom)
            if head_atom in undefined:
                undefined.remove(head_atom) # ...it is defined
        else:
            raise Exception(f"Could not get undefined atoms. Expected the head to have 1 atom; found {len(clause.left_head.atoms_here)}")
    return undefined

def get_set_of_abducibles(atoms: dict[str, Atom],  program: Program):
    abducibles = set() # returtn this
    undefined = get_undefined_atoms(atoms, program)
    for atom in undefined:
        fact = Rule(InfixExpression(atom.name, atoms), InfixExpression('T', atoms))
        assumption = Rule(InfixExpression(atom.name, atoms), InfixExpression('F', atoms))
        abducibles.add(fact)
        abducibles.add(assumption)
    
    for clause in program.clauses:
        if clause.non_nec == True:
            extra_abducible = Rule(clause.left_head, InfixExpression('T', atoms))
            abducibles.add(extra_abducible)
        
        if clause.factual == True:
            for atom in clause.right_body.atoms_here:
                if atom.is_abnormality:
                    extra_abducible = Rule(InfixExpression(f"{atom.name}", atoms), InfixExpression('T', atoms))
                    abducibles.add(extra_abducible)
    return abducibles

def generate_explanations(abducibles, atoms: dict[str, Atom]) ->list[Rule]:
    ''' 
    A explanation is a subset of the set of abducibles. 
    
    An explanation atom ← ⊥; atom ← T is condradictory

    Generate a list of non-contradictory explanations       
    '''
    explanations = []
    for abducible in abducibles:
        for explanation in explanations:
            contradictory = False
            for clause in explanation:
                if abducible.left_head == clause.left_head:
                    contradictory = True # we don't need contradictory explanations
            
            if not contradictory:
                extended_explanation = explanation.copy()
                extended_explanation.append(abducible)
                explanations.append(extended_explanation) # add a new explanation that is a copy of an existing explanation + this abducible
        explanations.append([abducible]) # add a new explanation that is just this abducible
    return explanations

def phi_with_abduction(explanations: list, program: Program,  observations: set[Observation], fixed_point: Interpretation, integrity_constraints: set[Rule]):
    result_interpretations = [] # the interpretation you get after running Phi with the added explanation
    fixed_point_clone = fixed_point.clone()
    for explanation in explanations:
        prog = Program(program.clauses + explanation).weakly_complete()
    
        abduced_interpretation = phi(prog, fixed_point_clone)
        next_phi = phi(prog, abduced_interpretation)
        while abduced_interpretation != next_phi:
            abduced_interpretation = next_phi.clone()
            next_phi = phi(prog, next_phi)
        
        # Check if abduced interpretation satisfies integrity constraints
        integrity_constraint_check_passed = True
        for constraint in integrity_constraints:
            if constraint.evaluate() != TruthConstant.TRUE:
                integrity_constraint_check_passed = False
                break
        if not integrity_constraint_check_passed:
            continue # this explanation is not correct. Try the next explanation

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
        if not explains_all:
            continue # this explanation is not correct. Try the next explanation

        # Make sure all abduced interpretation are minimal
        result_interpretations.append(abduced_interpretation)
        for interpretation in result_interpretations:
            if abduced_interpretation.isSuperset(interpretation):
                result_interpretations.remove(abduced_interpretation)
                break
            if abduced_interpretation.isSubset(interpretation):
                result_interpretations.remove(interpretation)

    return  result_interpretations 
    


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

