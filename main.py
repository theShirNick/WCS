import sys
import os
from collections import deque
from truth_constant import TruthConstant

from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase, QFont
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QFile, QIODevice, Slot

from qt_material import apply_stylesheet

from interpretation import Interpretation
from program import Program
from clauses import Rule
# from abductive_framework import Observation, credulous, explain_with_abduction, skeptical
from abductive_framework import Observation, get_set_of_abducibles, phi_with_abduction, skeptical
from infix.expression import InfixExpression
from phi import phi


if __name__ == "__main__":
    app = QApplication(sys.argv)

    ui_file_name = "ui/main.ui"
    ui_file = QFile(ui_file_name)
    if not ui_file.open(QIODevice.ReadOnly):
        print(f"Cannot open {ui_file_name}: {ui_file.errorString()}")
        sys.exit(-1)
    loader = QUiLoader()
    window = loader.load(ui_file)
    ui_file.close()
    if not window:
        print(loader.errorString())
        sys.exit(-1)

    # setup stylesheet
    QFontDatabase.addApplicationFont('ui/OverpassMono-Regular.ttf')
    apply_stylesheet(app, theme='dark_lightgreen.xml')

    # load custom style additions
    stylesheet = app.styleSheet()
    with open('ui/custom.css') as file:
        app.setStyleSheet(stylesheet + file.read().format(**os.environ))

    
    # force style 
    window.PTextEdit.setFont(QFont("Overpass Mono", 14))
    window.tabWidget.setCurrentIndex(1)
    window.wcPTextEdit.setFont(QFont("Overpass Mono", 14))
    window.tabWidget.setCurrentIndex(2)
    window.PhiTextEdit.setFont(QFont("Overpass Mono", 14))
    window.tabWidget.setCurrentIndex(3)
    window.ATextEdit.setFont(QFont("Overpass Mono", 14))
    window.tabWidget.setCurrentIndex(4)
    window.XTextEdit.setFont(QFont("Overpass Mono", 14))
    window.tabWidget.setCurrentIndex(0)

    window.observation_line_edit.setProperty('class', 'mono_font')
    window.constraint_body_line_edit.setProperty('class', 'mono_font')
    window.constraint_head_line_edit.setProperty('class', 'mono_font')
    window.input_program_text_edit.setProperty('class', 'background_active_input')
    window.clear_program_button.setProperty('class', 'danger')
    # window.undo_button.setProperty('class', 'warning')
    window.Phitab.setProperty('class', 'light_font')
    
    placeholder_text = "Enter clauses separated by a semicolon (;)\nAll clauses must be of the form \"body if head\"\nAbnormality predictes must begin with \"ab_\"\nPut an asterisk (*) in the body to make it a non-necessary antecedent.\nPut an asterisk (*) in the head to make it a factual conditional."
    window.input_program_text_edit.setPlaceholderText(placeholder_text)
    # Important stuff starts here
    atoms = dict()
    observations = set()
    clauses = []
    program = Program(clauses)
    wc_program = Program([]) 
    set_of_abducibles = list()
    interpretation_stack = deque()
    integrity_constraints = set()
    is_OR = False

    # Program Input
    @Slot()
    def input_program():
        program_text = window.input_program_text_edit.toPlainText().replace(':-', '←').replace('-:', '←').replace(' if ', '←')
        if len(program_text) == 0:
            return
        if program_text[-1] == ';':
            program_text = program_text[:-1]
        split_clauses = program_text.split(';')

        for clause in split_clauses:
            body, head = clause.split('←', 1)
            non_nec = False
            factual = False
            if '*' in body:
                non_nec = True
            if '*' in head:
                factual = True
            program.clauses.append(Rule(InfixExpression(body, atoms), InfixExpression(head, atoms), non_nec, factual))
        
        window.input_program_text_edit.setPlaceholderText("")
        window.input_program_text_edit.clear()
        window.PTextEdit.clear()
        window.PTextEdit.appendPlainText("𝓟:\n" + str(program))
        if len (observations) > 0:
            window.PTextEdit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.PTextEdit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))

        # Remake the other tabs
        wcP_Phi_X()

    # Connect the Program button to the function
    window.input_program_button.clicked.connect(input_program)
    
    # Observation Input
    @Slot()
    def input_observation():
        observation_expr = InfixExpression(window.observation_line_edit.text(), atoms)
        observations.add(Observation(observation_expr))

        window.observation_line_edit.clear()
        window.PTextEdit.clear()
        window.PTextEdit.appendPlainText("𝓟:\n" + str(program))
        if len (observations) > 0:
            window.PTextEdit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.PTextEdit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))

        # Remake the other tabs 
        wcP_Phi_X()

    # Connect the Observation button to the function
    window.input_observation_button.clicked.connect(input_observation)

    # Integrity Constraint Input
    @Slot()
    def input_IC():
        constraint_body_expr = InfixExpression(window.constraint_body_line_edit.text(), atoms)
        constraint_head_expr = InfixExpression(window.constraint_head_line_edit.text(), atoms)
        constraint = Rule(constraint_body_expr, constraint_head_expr)
        integrity_constraints.add(constraint)

        window.constraint_body_line_edit.clear()
        window.constraint_head_line_edit.clear()
        window.PTextEdit.clear()

        window.PTextEdit.appendPlainText("𝓟:\n" + str(program))
        if len (observations) > 0:
            window.PTextEdit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.PTextEdit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))
        # Remake the other tabs
        wcP_Phi_X()

    # Connect the Integrity Constraint button to the function
    window.input_constraint_button.clicked.connect(input_IC)

    # Conjunction Input
    @Slot()
    def input_conjunction():
        left_text = window.conjunction_line_edit_left.text()
        right_text = window.conjunction_line_edit_right.text()

        disjunction_of_negated = InfixExpression(f'~{left_text} and ~{right_text}', atoms)
        constraint = Rule(InfixExpression("F", atoms), disjunction_of_negated)
        integrity_constraints.add(constraint)
        if not is_OR:
            disjunction_for_exclusive = InfixExpression(f'{left_text} and {right_text}', atoms)
            constraint = Rule(InfixExpression("F", atoms), disjunction_for_exclusive)
            integrity_constraints.add(constraint)

        window.conjunction_line_edit_left.clear()
        window.conjunction_line_edit_right.clear()
        window.PTextEdit.clear()

        window.PTextEdit.appendPlainText("𝓟:\n" + str(program))
        if len (observations) > 0:
            window.PTextEdit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.PTextEdit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))
        # Remake the other tabs
        wcP_Phi_X()

    # Connect the Integrity Constraint button to the function
    window.input_conjunction_button.clicked.connect(input_conjunction)

    # Weakly Complete and construct text
    def wcP(): 
        global wc_program
        wc_program = program.weakly_complete()
        window.wcPTextEdit.clear()
        window.wcPTextEdit.appendPlainText("𝔀𝓬𝓟:\n" + str(wc_program))
        if len (observations) > 0:
            window.wcPTextEdit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.wcPTextEdit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))

    # Semantic Phi Operator
    def phi_fixed_point():
        interpretation_stack.clear()
        window.PhiTextEdit.clear()
        if len(interpretation_stack) == 0:
            interpretation_stack.append(Interpretation(set(), set(), set(atoms.values() )))
        
        stop = False
        while stop == False:
            window.PhiTextEdit.appendPlainText(f"Φ↑{len(interpretation_stack) -1}: {str(interpretation_stack[-1])}")
            next_phi = phi(wc_program, interpretation_stack[-1])
            if interpretation_stack[-1] == next_phi:
                window.PhiTextEdit.appendPlainText(f"Fixed point found.\n")
                stop = True

                integrity_constraint_check = True
                for constraint in integrity_constraints:
                    if constraint.evaluate() != TruthConstant.TRUE:
                        integrity_constraint_check = False
                        break
                if not integrity_constraint_check:
                    window.PhiTextEdit.appendPlainText(f"Integrity constraint not satisfied. Try abduction.\n")

                unexplained = set()
                for ob in observations:
                    if ob.atom in next_phi.unknowns:
                        unexplained.add(ob)
                if len(unexplained) > 0:
                    window.PhiTextEdit.appendPlainText("Observations %s"%( observations ) + " are unexplained. Abduction may help.")

            else:     
                interpretation_stack.append(next_phi) 

    # 𝒜 - set of abducibles
    def get_A():  
        global set_of_abducibles
        set_of_abducibles = get_set_of_abducibles(atoms, wc_program)
        window.ATextEdit.clear()
        if len(set_of_abducibles) == 0:
            window.ATextEdit.appendPlainText('All atoms are defined')
        else:
            window.ATextEdit.appendPlainText(f'𝒜:')
            for explanation in set_of_abducibles:
                window.ATextEdit.appendPlainText(f'{str(explanation)},')
        
    # 𝒳 - explain with abduction
    def abduction():  
        window.XTextEdit.clear()
        abduced_models = set()
        if len(interpretation_stack) > 0:
            # abduced_models = explain_with_abduction(atoms, wc_program, observations, interpretation_stack[-1], integrity_constraints)
            
            abduced_models = phi_with_abduction(set_of_abducibles, wc_program, observations, interpretation_stack[-1], integrity_constraints)
            if len(abduced_models) > 0:
                skeptical_result = skeptical(atoms,wc_program, abduced_models)
                window.XTextEdit.appendPlainText(skeptical_result)
                window.XTextEdit.appendPlainText(f'\nAbduced models:')
                for abd_model in abduced_models:
                    window.XTextEdit.appendPlainText(str(abd_model))
            else:
                window.XTextEdit.appendPlainText(f"Abduction yielded nothing.\nThe answer is still {str(interpretation_stack[-1])}")
        else:
            window.XTextEdit.appendPlainText("ERROR: Interpretation stack empty. Did Phi run correctly?")

     # Exclusive conjunction button
    @Slot()
    def XOR_switch():
        global is_OR
        is__OR = window.exclusive_button.isChecked()
        if is__OR:
            window.exclusive_button.setText("OR")
        else:
            window.exclusive_button.setText("XOR")  
    # Connect the XOR switch to the function  
    window.exclusive_button.clicked.connect(XOR_switch)

    # Call this after input changes
    def wcP_Phi_X():
        wcP()
        phi_fixed_point()
        get_A()
        abduction()
        
    # Clear Program
    @Slot()
    def clear_program():
        window.input_program_text_edit.setPlaceholderText(placeholder_text)
        clauses.clear()
        atoms.clear()
        observations.clear()
        integrity_constraints.clear()
        set_of_abducibles.clear()
        interpretation_stack.clear()
        window.PTextEdit.clear()
        window.wcPTextEdit.clear()
        window.PhiTextEdit.clear()
        window.XTextEdit.clear()
        window.ATextEdit.clear()
        
       
    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    # run GUI
    window.show()
    sys.exit(app.exec_())