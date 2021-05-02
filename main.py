import sys
import os
from collections import deque

from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QFile, QIODevice, Slot

from qt_material import apply_stylesheet

from interpretation import Interpretation
from program import Program
from clauses import Rule
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
    window.input_program_text_edit.setProperty('class', 'mono_font')
    window.input_program_text_edit.setProperty('class', 'background_active_input')
    window.rule_head_line_edit.setProperty('class', 'mono_font')
    window.rule_body_line_edit.setProperty('class', 'mono_font')
    window.clear_program_button.setProperty('class', 'danger')
    window.undo_button.setProperty('class', 'warning')
    window.phi_plus_button.setProperty('class', 'light_font')
    window.wcP_button.setProperty('class', 'light_font')

    # Important stuff starts here
    atoms = dict()
    clauses = []
    program = Program(clauses)
    wc_program = Program([])
    interpretation_stack = deque()

    # Rule Input
    @Slot()
    def input_clause():
        rule_body_text = InfixExpression( window.rule_body_line_edit.text(), atoms)
        rule_head_text = InfixExpression(window.rule_head_line_edit.text(), atoms)
        rule = Rule(rule_body_text, rule_head_text)
        clauses.append(rule)

        window.rule_body_line_edit.clear()
        window.rule_head_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))
    # Connect the Rule button to the function
    window.input_clause_button.clicked.connect(input_clause)

    # Rule Input
    @Slot()
    def input_program():
        program_text = window.input_program_text_edit.toPlainText().replace(':-', '←').replace('-:', '←').replace(' if ', '←')

        split_clauses = program_text.split(',')

        for clause in split_clauses:
            body, head = clause.split('←', 1)
            program.clauses.append(Rule(InfixExpression(body, atoms), InfixExpression(head, atoms)))
        

        window.input_program_text_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))
    # Connect the Rule button to the function
    window.input_program_button.clicked.connect(input_program)

    # wcP Button
    @Slot()
    def wcP():
        global wc_program
        wc_program = program.weakly_complete()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(wc_program))
    # Connect the wcP button to the function
    window.wcP_button.clicked.connect(wcP)

    # Clear Program
    @Slot()
    def clear_program():
        clauses.clear()
        atoms.clear()
        window.output_text_edit.clear()
        interpretation_stack.clear()
    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    @Slot()
    def phi_plus():
        window.output_text_edit.clear()
        if len(wc_program.clauses) == 0:
            window.output_text_edit.appendPlainText("Don't forget to weakly complete your program!")
            return
        else:
            if len(interpretation_stack) == 0:
                interpretation_stack.append(Interpretation(set(), set(), set(atoms.values() )))
            window.output_text_edit.appendPlainText(f"Φ iteration {len(interpretation_stack) -1} with\n{str(interpretation_stack[-1])}")
            next_phi = phi(wc_program, interpretation_stack[-1])
            if interpretation_stack[-1] == next_phi:
                window.output_text_edit.appendPlainText(f"Fixed point found")
            else:     
                interpretation_stack.append(next_phi)
                window.output_text_edit.appendPlainText(f"The consequence is {str(next_phi)}")   
    # Connect the Φ++ button to the function
    window.phi_plus_button.clicked.connect(phi_plus)

    
    # run GUI
    window.show()
    sys.exit(app.exec_())