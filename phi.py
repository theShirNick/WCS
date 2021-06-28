from program import Program
from interpretation import Interpretation
from truth_constant import TruthConstant

def phi(wc_program:Program, interpretation:Interpretation) -> Interpretation:

    new_interpretation = interpretation.clone()
    candidates_for_false = dict()
    to_true = set()
    # get immediate consequences
    for clause in wc_program.clauses:
        if len(clause.left_head.atoms_here) == 1: # then this is a valid clause. find consequence
            consequence_atom = clause.left_head.atoms_here.pop()
            clause.left_head.atoms_here.add(consequence_atom)

            if clause.right_body.evaluate() == TruthConstant.TRUE:
                to_true.add(consequence_atom)

                if consequence_atom in candidates_for_false:
                    candidates_for_false[consequence_atom] = False

            elif clause.right_body.evaluate() == TruthConstant.FALSE:
                if consequence_atom not in candidates_for_false:
                    candidates_for_false[consequence_atom] = True
            elif clause.right_body.evaluate() == TruthConstant.UNKNOWN:
                if consequence_atom in candidates_for_false:
                    candidates_for_false[consequence_atom] = False
        elif len(clause.left_head.atoms_here) == 0: # maybe this is an integrity constraint 
            raise Exception(f'Could not get immediate consequence for {clause}. There must be 1 atom in the body but found 0. Integrity constraints are not part of the program and should be declared separately.')
        else:
            raise Exception(f'Could not get immediate consequence for {clause}. There must be 1 atom in the body; but found {len(clause.left_head.atoms_here)}.')

    for atom in to_true:
        atom.ground_value = TruthConstant.TRUE
        if atom in new_interpretation.unknowns:
            new_interpretation.unknowns.remove(atom)
            new_interpretation.trues.add(atom)
        if atom in new_interpretation.falses:
            new_interpretation.falses.remove(atom)
            new_interpretation.trues.add(atom)
        
    for atom in candidates_for_false:
        if candidates_for_false[atom] == True:
            atom.ground_value = TruthConstant.FALSE
            if atom in new_interpretation.unknowns:
                new_interpretation.unknowns.remove(atom)
                new_interpretation.falses.add(atom)
   
    return new_interpretation