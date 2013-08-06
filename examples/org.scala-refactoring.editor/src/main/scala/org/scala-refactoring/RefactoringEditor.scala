package scala.tools.refactoring

import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JTextArea
import javax.swing.JOptionPane

import tools.refactoring.util.CompilerProvider
import tools.refactoring.analysis.GlobalIndexes
import tools.refactoring.common.Change
import tools.refactoring.implementations._
import tools.refactoring._

object RefactoringEditor extends EditorUi {

  def main(args: Array[String]) {}

  getJFrame setVisible true

  getRenameMenuItem addActionListener new ActionListener {
    def actionPerformed(e: ActionEvent) = renameAction(getEditorTextArea)
  }

  getOrganizeMenuItem addActionListener new ActionListener {
    def actionPerformed(e: ActionEvent) = organizeAction(getEditorTextArea)
  }

//  TODO: Implement the other refactorings.

  def askNewName(currentName: String) = {
    JOptionPane.showInputDialog(
        getJFrame,
        "Please enter a new name:",
        "Rename",
        JOptionPane.PLAIN_MESSAGE,
        null /*icon*/,
        null /*choices*/,
        currentName) match {
      case s: String => s
      case _ => currentName
    }
  }

  def showError(msg: String) = {
    JOptionPane.showMessageDialog(getJFrame, msg, "An Error Occured", JOptionPane.ERROR_MESSAGE)
  }

  def renameAction(editor: JTextArea) {

    val refactoring = new Rename with CompilerProvider with GlobalIndexes {
      val ast = treeFrom(editor.getText)
      val index = GlobalIndex(ast)
    }

    val selection: refactoring.Selection = {
      val file = refactoring.ast.pos.source.file
      val from = editor.getSelectionStart
      val to = editor.getSelectionEnd
      refactoring.FileSelection(file, from, to)
    }

    val preparationResult = refactoring.prepare(selection) match {
      case Left(refactoring.PreparationError(error)) =>
        showError(error)
        return
      case Right(r) => r
    }

    val refactoringParameters = {
      val selectedName = preparationResult.selectedTree.symbol.nameString
      askNewName(selectedName)
    }

    val changes: List[Change] = refactoring.perform(selection, preparationResult, refactoringParameters) match {
      case Left(refactoring.RefactoringError(error)) =>
        showError(error)
        return
      case Right(r) => r
    }

    editor setText Change.applyChanges(changes, editor.getText)
  }

  def organizeAction(editor: JTextArea) {

    val src = editor.getText

    val refactoring = new OrganizeImports with CompilerProvider{
      val ast = treeFrom(src)
    }

    val selection = refactoring.TreeSelection(refactoring.ast)

    val changes = refactoring.perform(selection, new refactoring.PreparationResult, new refactoring.RefactoringParameters) match {
      case Left(refactoring.RefactoringError(error)) =>
        showError(error)
        return
      case Right(r) => r
    }

    editor setText Change.applyChanges(changes, src)
  }
}
