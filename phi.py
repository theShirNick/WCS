from program import Program
from interpretation import Interpretation
from truth_constant import TruthConstant

def phi(program:Program, interpretation:Interpretation) -> Interpretation:

    new_interpretation = interpretation.clone()
    candidates_for_false = dict()
    to_true = set()
    # get immediate consequences
    for clause in program.clauses:
        if len(clause.body.atoms_here) == 1:
            consequence_atom = clause.body.atoms_here.pop()
            clause.body.atoms_here.add(consequence_atom) # pop then add. looks hacky

            if clause.head.evaluate() == TruthConstant.TRUE:
                to_true.add(consequence_atom)
                                
                if consequence_atom in candidates_for_false:
                    candidates_for_false[consequence_atom] = False

            elif clause.head.evaluate() == TruthConstant.FALSE:
                if consequence_atom not in candidates_for_false:
                    candidates_for_false[consequence_atom] = True
            elif clause.head.evaluate() == TruthConstant.UNKNOWN:
                if consequence_atom in candidates_for_false:
                    candidates_for_false[consequence_atom] = False
        else:
            raise Exception(f'Could not get immediate consequence for {clause}. There must be 1 atom in the body; but found {len(clause.body.atoms_here)}.')

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