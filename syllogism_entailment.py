from interpretation import Interpretation
from CONSTANTS import *

def extract_predicate(term:str) -> str:
    result = ''
    index = term.find(PREDICATE_FONT)+len(PREDICATE_FONT)
    char = term[index]
    while char != '<':
        result = result + char
        index = index + 1
        char = term[index]
    return result

def extract_argument(term:str) -> str:
    result = ''
    index = term.find(ARGUMENT_FONT)+len(ARGUMENT_FONT)
    char = term[index]
    while char != '<':
        result = result + char
        index = index + 1
        char = term[index]
    return result


def get_entailment_of_syllogism(interpretation: Interpretation, syllogism_mood: str, syllogism_y: str, syllogism_z: str) -> bool:
    '''
        Given a model interpretation and a syllogism of the form 'some y are z', where some is the mood, find what the syllogism entails 
    '''

    if syllogism_mood == "All (A)":
        '''
            P entails Ayz if and only if we find an object which belongs to the class y 
            and for all objects belonging to the class y we find that they also belong to class z
        ''' 
        objects_belonging_to_y = set()
        for true_y in interpretation.trues:
            if syllogism_y == extract_predicate(true_y):
                objects_belonging_to_y.add(extract_argument(true_y))
        if len(objects_belonging_to_y) == 0:
            return False
        
        for o in objects_belonging_to_y:
            found_z_o = False
            for true_z in interpretation.trues:
                if extract_predicate(true_z) == syllogism_z and extract_argument(true_z) == o:
                    found_z_o = True
            if not found_z_o:
                return False
        return True
    elif syllogism_mood == "Some (I)":
        '''
            P entails Iyz if and only if we find objects o1, o2, and o3 such that
            o1 belongs to the classes y and z, 
            o2 belongs to the class y and it is unknown whether it belongs to the class z, 
            and o3 belongs to the class z and it is unknown whether it belongs to the class y.
        '''
        pass
    
    elif syllogism_mood == "No (E)":
        '''
            P entails Eyz if and only if we find an object belonging to the class y 
            and for all objects belonging to the class y we find that they do not belong to the class z.
        '''

        objects_belonging_to_y = set()
        for true_y in interpretation.trues:
            if syllogism_y == extract_predicate(true_y):
                objects_belonging_to_y.add(extract_argument(true_y))
        if len(objects_belonging_to_y) == 0:
            return False
        
        for o in objects_belonging_to_y:
            for true_z in interpretation.trues:
                if extract_predicate(true_z) == syllogism_z and extract_argument(true_z) == o:
                    return False
        return True
        

    elif syllogism_mood == "Some (O)":
        '''
            P entails Oyz if and only if we find objects o1 and o2 such that 
            o1 belongs to the class y and does not belong to the class z, 
            whereas o2 belongs to the class y and it is unknown whether it does not belong to the class z.

        '''
        pass

                    
            


    # ERROR
    return False