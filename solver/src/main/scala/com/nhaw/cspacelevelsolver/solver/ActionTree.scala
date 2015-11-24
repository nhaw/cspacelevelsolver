package com.nhaw.cspacelevelsolver.solver

import scala.collection.Iterator
import java.io.PrintStream

import scala.collection._


case class ActionTreeNode(action: Action, paths: Seq[ActionTreeNode]) {
  def display(o: PrintStream) {
    _display(o, this, 0)
  }

  private[this] def _display(o: PrintStream, atn: ActionTreeNode, indent: Int) {
    o.println(" "*indent + atn.action.toString + ":")
    atn.paths.foreach(_display(o, _, indent+2))
  }

  def solutions: Iterator[List[ActionTreeNode]] = {
    //    class Iter extends Iterator[List[ActionTreeNode]] {
    //      private[this]
    //
    //      def hasNext = false
    //      def next = null
    //    }
    //    new Iter()

    val stack = List[ActionTreeNode](this)
    var iter = Iterator[List[ActionTreeNode]]()

    def recurse(stack: List[ActionTreeNode], n: ActionTreeNode) {
      n.action match {
        case a: EndAction => iter ++= Seq(stack)
        case a => n.paths.foreach { child => recurse(n :: stack, child) }
      }
    }
    recurse(stack, this)
    iter
  }

  def numSolutions: Int = action match {
    case a: EndAction => 1
    case a => paths.foldLeft(0)((sum:Int,n:ActionTreeNode) => sum + n.numSolutions)
  }

  /**
   * @note should not need to be recursive if no incomplete paths are generated in the tree
   * @return Does this tree solve the com.nhaw.cspacelevelsolver.color.puzzle
   */
  def solves: Boolean = numSolutions > 0
}
