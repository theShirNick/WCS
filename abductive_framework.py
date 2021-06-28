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
        Get atoms that do not occur in the head of any clause
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

def generate_explanations(abducibles, atoms: dict[str, Atom]) ->list[set[Rule]]:
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
                extended_explanation.add(abducible)
                explanations.append(extended_explanation) # add a new explanation that is a copy of an existing explanation + this abducible
        explanations.append({abducible}) # add a new explanation that is just this abducible
    explanations.sort(key=len)
    return explanations

def phi_with_abduction(explanations: list[set[Rule]], program: Program,  observations: set[Observation], fixed_point: Interpretation, integrity_constraints: set[Rule]):
    '''
        Continue the semantic operator from the least fixed point by adding each explanation. Discard everything but minimal models.  
    '''

    fixed_point_clone = fixed_point.clone()
    explanation_interpretation = list() # of tuples, containing a valid explanation and the correcponding model
    for explanation in explanations:

        redundant = False
        for existing_explanation, interpretation in explanation_interpretation:
            if explanation.issuperset(existing_explanation):
                redundant = True # a smaller valid explanation exists
                break
        if redundant:
            continue # this explanation is redundant. No need to go on with it

        # We are here only if the explanation is minimal. Let's see if it actually explains everything
        #
        # Find the new least fixed point, now that we added the explanation to our program
        prog = Program(program.clauses + list(explanation)).weakly_complete()
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

        # We are here only if the explanation is valid and minimal. Add it!
        explanation_interpretation.append((explanation, abduced_interpretation))
           
    return  explanation_interpretation 
    


def skeptical(atoms: dict[str, Atom],  program: Program, abduced_interpretations:list[Interpretation]) :
    undefined = get_undefined_atoms(atoms, program)
    all_skeptical_trues = set()
    all_skeptical_falses = set()
    if len(abduced_interpretations) > 0:
        all_skeptical_trues = abduced_interpretations[0].trues.copy()
        all_skeptical_falses = abduced_interpretations[0].falses.copy()

    for interpretation in abduced_interpretations:
        all_skeptical_trues = all_skeptical_trues.intersection(interpretation.trues)
        all_skeptical_falses = all_skeptical_falses.intersection(interpretation.falses)

    new_trues_only = all_skeptical_trues.intersection(undefined)
    new_falses_only = all_skeptical_falses.intersection(undefined)

    old_trues_only = all_skeptical_trues.difference(new_trues_only)
    old_falses_only = all_skeptical_falses.difference(new_falses_only)    

    return_str = 'Skeptically, '

    # format the representation of old true and false atoms
    old_s = ''
    for a in old_trues_only:
        old_s = old_s + f"{str(a)}, "
    if len(old_trues_only) > 0 and len(old_falses_only) == 0:
        old_s = old_s[:-2]
    for a in old_falses_only:
        old_s = old_s + f"¬{str(a)}, "
    if len(old_falses_only) > 0:
        old_s = old_s[:-2]

    # format the representation of new true and false atoms
    new_s = ''
    for a in new_trues_only:
        new_s = new_s + f"{str(a)}, "
    if len(new_trues_only) > 0 and len(new_falses_only) == 0:
        new_s = new_s[:-2]
    for a in new_falses_only:
        new_s = new_s + f"¬{str(a)}, "
    if len(new_falses_only) > 0:
        new_s = new_s[:-2]
    
    if len(new_trues_only) == 0 and len(new_falses_only) == 0:
        return_str = return_str + f"nothing new follows. We already know:\n{old_s}"
    else:
        return_str = return_str + f"it follows:\n{new_s}\n in addition to what we already know:\n{old_s}"
    

    return return_str

