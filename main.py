import sys
import os
from collections import deque
from truth_constant import TruthConstant

from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QFile, QIODevice, Slot

from qt_material import apply_stylesheet

from interpretation import Interpretation
from program import Program
from clauses import Rule
from abductive_framework import Observation, credulous, explain_with_abduction, skeptical
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
    window.output_text_edit.setProperty('class', 'mono_font')
    window.observation_line_edit.setProperty('class', 'mono_font')
    window.constraint_body_line_edit.setProperty('class', 'mono_font')
    window.constraint_head_line_edit.setProperty('class', 'mono_font')
    window.input_program_text_edit.setProperty('class', 'background_active_input')
    window.clear_program_button.setProperty('class', 'danger')
    window.undo_button.setProperty('class', 'warning')
    window.phi_plus_button.setProperty('class', 'light_font')
    window.wcP_button.setProperty('class', 'light_font')

    # Important stuff starts here
    atoms = dict()
    observations = set()
    clauses = []
    program = Program(clauses)
    wc_program = Program([]) 
    interpretation_stack = deque()
    integrity_constraints = set()
    wcP_pressed = False

    # Program Input
    @Slot()
    def input_program():
        program_text = window.input_program_text_edit.toPlainText().replace(':-', '←').replace('-:', '←').replace(' if ', '←')

        split_clauses = program_text.split(',')

        for clause in split_clauses:
            body, head = clause.split('←', 1)
            program.clauses.append(Rule(InfixExpression(body, atoms), InfixExpression(head, atoms)))
        

        window.input_program_text_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText("𝓟:\n" + str(program))
        if len (observations) > 0:
            window.output_text_edit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.output_text_edit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))
    # Connect the Program button to the function
    window.input_program_button.clicked.connect(input_program)
    
    # Observation Input
    @Slot()
    def input_observation():
        observation_expr = InfixExpression(window.observation_line_edit.text(), atoms)
        observations.add(Observation(observation_expr))

        window.observation_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText("𝓟:\n" + str(program))
        if len (observations) > 0:
            window.output_text_edit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.output_text_edit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))
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
        window.output_text_edit.clear()

        window.output_text_edit.appendPlainText("𝓟:\n" + str(program))
        if len (observations) > 0:
            window.output_text_edit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.output_text_edit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))

    # Connect the Observation button to the function
    window.input_constraint_button.clicked.connect(input_IC)

    # wcP Button
    @Slot()
    def wcP():
        global wcP_pressed
        wcP_pressed = True
        global wc_program
        wc_program = program.weakly_complete()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText("𝔀𝓬𝓟:\n" + str(wc_program))
        if len (observations) > 0:
            window.output_text_edit.appendPlainText("𝓞:\n%s"%( observations ))
        if len (integrity_constraints) > 0:
            window.output_text_edit.appendPlainText("𝓘𝓒:\n%s"%( integrity_constraints ))

    # Connect the wcP button to the function
    window.wcP_button.clicked.connect(wcP)

    # 𝒳 - explain with abduction Button
    @Slot()
    def abduction():
        
        window.output_text_edit.clear()
        abduced_models = set()
        if len(interpretation_stack) > 0:
            abduced_models = explain_with_abduction(atoms, wc_program, observations, interpretation_stack[-1], integrity_constraints)
            if len(abduced_models) > 0:
                credulous_result = credulous(abduced_models)
                skeptical_result = skeptical(abduced_models)
                window.output_text_edit.appendPlainText(credulous_result)
                window.output_text_edit.appendPlainText(skeptical_result)
                window.output_text_edit.appendPlainText(f'\nAbduced models:\n{abduced_models}')


            else:
                window.output_text_edit.appendPlainText("Abduction yielded nothing")
        else:
            window.output_text_edit.appendPlainText("Be sure to find the fixed point first")

    # Connect the X button to the function
    window.abduction_button.clicked.connect(abduction)

    # Clear Program
    @Slot()
    def clear_program():
        clauses.clear()
        atoms.clear()
        observations.clear()
        integrity_constraints.clear()
        interpretation_stack.clear()
        window.output_text_edit.clear()
        wcP_pressed = False
        
    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    # Semantic Phi Operator
    @Slot()
    def phi_plus():
        window.output_text_edit.clear()
        if len(wc_program.clauses) == 0 and  not wcP_pressed:
            window.output_text_edit.appendPlainText("Don't forget to weakly complete your program!")
            return
        else:
            if len(interpretation_stack) == 0:
                interpretation_stack.append(Interpretation(set(), set(), set(atoms.values() )))
            window.output_text_edit.appendPlainText(f"Φ iteration {len(interpretation_stack) -1} with\n{str(interpretation_stack[-1])}")
            next_phi = phi(wc_program, interpretation_stack[-1])
            if interpretation_stack[-1] == next_phi:
                window.output_text_edit.appendPlainText(f"Fixed point found.\n")

                integrity_constraint_check = True
                for constraint in integrity_constraints:
                    if constraint.evaluate() != TruthConstant.TRUE:
                        integrity_constraint_check = False
                        break
                if not integrity_constraint_check:
                    window.output_text_edit.appendPlainText(f"Integrity constraint not satisfied. Try abduction.\n")

                unexplained = set()
                for ob in observations:
                    if ob.atom in next_phi.unknowns:
                        unexplained.add(ob)
                if len(unexplained) > 0:
                    window.output_text_edit.appendPlainText("Observations %s"%( observations ) + " are unexplained. Abduction may help.")

            else:     
                interpretation_stack.append(next_phi)
                window.output_text_edit.appendPlainText(f"The consequence is {str(next_phi)}")   
    # Connect the Φ++ button to the function
    window.phi_plus_button.clicked.connect(phi_plus)

    
    # run GUI
    window.show()
    sys.exit(app.exec_())