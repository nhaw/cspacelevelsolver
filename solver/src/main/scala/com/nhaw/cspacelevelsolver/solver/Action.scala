package com.nhaw.cspacelevelsolver.solver

import java.io.PrintStream

import com.nhaw.cspacelevelsolver.color.Color
import com.nhaw.cspacelevelsolver.puzzle.{Entity, PuzzleState}

import scala.collection._

import _root_.scala.collection.immutable.Seq


/**
 * Action for progressing through a com.nhaw.cspacelevelsolver.color.puzzle. e.g. use an com.nhaw.cspacelevelsolver.color on an entity
 */
trait Action {
  def resultState(st: PuzzleState): PuzzleState
}

case class RootAction() extends Action {
  def resultState(st: PuzzleState) = ???
}

case class EndAction() extends Action {
  def resultState(st: PuzzleState) = ???
}

case class NoAction() extends Action {
  def resultState(st: PuzzleState) = st
}

case class ColorChangeAction(color: Color, ent: Entity) extends Action {
  def resultState(st: PuzzleState) = {
    val newInv = {
      var found = false
      st.inventory.filter { c => {
        if (found)
          true
        else if (c != color)
          true
        else {
          found = false
          false
        }
      }
      }
    }
    new PuzzleState(newInv, st.world + (ent -> st.world(ent).withColor(color) ))
  }
}

case class ActionTreeNode(action: Action, paths: Seq[ActionTreeNode]) {
  def display(o: PrintStream) {
    _display(o, this, 0)
  }

  private[this] def _display(o: PrintStream, atn: ActionTreeNode, indent: Int) {
    o.println(" "*indent + atn.action.toString + ":")
    atn.paths.foreach(_display(o, _, indent+2))
  }

  def solutions: Int = action match {
    case a: EndAction => 1
    case a => paths.foldLeft(0)((sum:Int,n:ActionTreeNode) => sum + n.solutions)
  }

  /**
   * @note should not need to be recursive if no incomplete paths are generated in the tree
   * @return Does this tree solve the com.nhaw.cspacelevelsolver.color.puzzle
   */
  def solves: Boolean = solutions > 0
}
