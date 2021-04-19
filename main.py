import sys
import os
from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QFile, QIODevice, Slot

from qt_material import apply_stylesheet

from program import *


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
    QFontDatabase.addApplicationFont('ui/JAi_____.TTF')
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
    window.clear_program_button.setProperty('class', 'danger')
    window.undo_button.setProperty('class', 'warning')
    

    clauses = []
    program = Program(clauses)

    # Fact Input
    @Slot()
    def add_fact():
        fact_text = window.fact_line_edit.text()
        fact = Fact(Atom(name=fact_text))
        clauses.append(fact)
        program = Program(clauses)

        window.fact_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))

    # Connect the Fact button to the function
    window.add_fact_button.clicked.connect(add_fact)

    # Assumption Input
    @Slot()
    def add_assumption():
        assumption_text = window.assumption_line_edit.text()
        assumption = Assumption(Atom(name=assumption_text))
        clauses.append(assumption)
        program = Program(clauses)

        window.assumption_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))

    # Connect the Assumption button to the function
    window.add_assumption_button.clicked.connect(add_assumption)

    # Rule Input
    @Slot()
    def add_rule():
        rule_body_text = window.rule_body_line_edit.text()
        rule_head_text = window.rule_head_line_edit.text()
        rule = Rule(Atom(name=rule_body_text), Atom(name=rule_head_text))
        clauses.append(rule)
        program = Program(clauses)

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
        program = Program(clauses)
        window.output_text_edit.clear()

    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    
    # run GUI
    window.show()
    sys.exit(app.exec_())