import itertools
from interpretation import Interpretation
import sys
import os
import re


from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QFile, QIODevice, Slot

from qt_material import apply_stylesheet

import itertools

from program import *
# from interpretation import *


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
    window.fact_line_edit.setProperty('class', 'mono_font')
    window.assumption_line_edit.setProperty('class', 'mono_font')
    window.rule_head_line_edit.setProperty('class', 'mono_font')
    window.rule_body_line_edit.setProperty('class', 'mono_font')
    window.fact_label.setProperty('class', 'mono_font')
    window.assumption_label.setProperty('class', 'mono_font')
    window.clear_program_button.setProperty('class', 'danger')
    window.undo_button.setProperty('class', 'warning')
    
    atoms = dict()
    clauses = []
    program = Program(clauses)

    def process_input(input: str):
        '''
        UNFINISHED

        Recognize atoms in input: add new ones, reuse old ones.
        '''

        if input not in atoms:
            atoms[input] = Atom(name=input)

    # Fact Input
    @Slot()
    def add_fact():
        fact_text = window.fact_line_edit.text().strip(' ')
        process_input(fact_text)
        fact = Fact(atoms[fact_text])
        clauses.append(fact)

        window.fact_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))

    # Connect the Fact button to the function
    window.add_fact_button.clicked.connect(add_fact)

    # Assumption Input
    @Slot()
    def add_assumption():
        assumption_text = window.assumption_line_edit.text().strip(' ')
        process_input(assumption_text)
        assumption = Assumption(atoms[assumption_text])      
        clauses.append(assumption)

        window.assumption_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))

    # Connect the Assumption button to the function
    window.add_assumption_button.clicked.connect(add_assumption)

    # Rule Input
    @Slot()
    def add_rule():
        rule_body_text = window.rule_body_line_edit.text().strip(' ')
        process_input(rule_body_text)
        rule_head_text = window.rule_head_line_edit.text().strip(' ')
        process_input(rule_head_text)
        rule = Rule(atoms[rule_body_text], atoms[rule_head_text])
        clauses.append(rule)

        window.rule_body_line_edit.clear()
        window.rule_head_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))

    # Connect the Rule button to the function
    window.add_rule_button.clicked.connect(add_rule)

    # Clear Program
    @Slot()
    def clear_program():
        clauses.clear()
        atoms.clear()
        window.output_text_edit.clear()

    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    # Test "All False" Interpretation <<<<<<<<<< TODO remove
    @Slot()
    def test_all_false_interpretation():
        interp = Interpretation(set(), set(atoms.values()), set())
        window.output_text_edit.clear()
        if interp.isModel(program):
            window.output_text_edit.appendPlainText(f"{str(interp)}\n ⊨ Is a model for\n{str(program)}")
        else:
           window.output_text_edit.appendPlainText(f"{str(interp)}\n ⊭ Is not a model for\n{str(program)}")
    # Connect the All False button to the function
    window.clever_button.clicked.connect(test_all_false_interpretation)

    
    # run GUI
    window.show()
    sys.exit(app.exec_())