from program import Program
from interpretation import Interpretation
from truth_constant import TruthConstant

def phi(program:Program, interpretation:Interpretation) -> Interpretation:

    new_interpretation = interpretation.clone()
    candidates_for_false = dict()
    # get immediate consequences
    for clause in program.clauses:
        if len(clause.body.atoms_here) == 1:
            consequence_atom = clause.body.atoms_here.pop()
            clause.body.atoms_here.add(consequence_atom) # pop then add. looks hacky

            if clause.head.evaluate() == TruthConstant.TRUE:
                consequence_atom.ground_value = TruthConstant.TRUE
                if consequence_atom in new_interpretation.unknowns:
                    new_interpretation.unknowns.remove(consequence_atom)
                    new_interpretation.trues.add(consequence_atom)
                if consequence_atom in new_interpretation.falses:
                    new_interpretation.falses.remove(consequence_atom)
                    new_interpretation.trues.add(consequence_atom)
                
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
            
    for atom in candidates_for_false:
        if candidates_for_false[atom] == True:
            atom.ground_value = TruthConstant.FALSE
            if atom in new_interpretation.unknowns:
                new_interpretation.unknowns.remove(atom)
                new_interpretation.falses.add(atom)
   
    return new_interpretation