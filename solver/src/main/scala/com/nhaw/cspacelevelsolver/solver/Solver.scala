package com.nhaw.cspacelevelsolver.solver

import com.nhaw.cspacelevelsolver.puzzle._

import collection._

trait SolverEvent

/**
 * A node has been reached in a possible solution path. This is counted once for each node and unique path leading up to
 * that node, regardless of whether it leads to a solution or not
 */
case class NodeReached(node: Node, depth: Int) extends SolverEvent

/**
 * The puzzle has been solved. The Node at which the end of the puzzle has been reached is the most recent NodeReached
 * object
 */
case class PuzzleSolved() extends SolverEvent

/**
 * No transition to this node from the previous node (indicated by the most recent NodeReached object)
 */
case class NoTransitionPossible(node: Node) extends SolverEvent

/**
 * No possible transitions can be made to get out of a given node. The node is indicated by the most revent NodeReached
 * object
 */
case class DeadEndReached() extends SolverEvent

//case class Cycle() extends SolverEvent


object CountingTracer {
  def apply() = new CountingTracer
}

class CountingTracer extends Function[SolverEvent,Unit] {
  private[this] var _nodesReached = 0
  private[this] var _solutions = 0
  private[this] var _deadEnds = 0

  def nodesReached = _nodesReached
  def solutions = _solutions
  def deadEnds = _deadEnds

  override def toString = s"CountingTracer(nodes:$nodesReached, solutions:$solutions, dead-ends:$deadEnds)"

  def apply(arg: SolverEvent): Unit = {
    arg match {
      case NodeReached(node, depth) => _nodesReached += 1
      case PuzzleSolved() => _solutions += 1
      case DeadEndReached() => _deadEnds += 1
    }
  }
}

class SolverMetrics
// TODO: Implement this

abstract class Solver(puzzle: Puzzle) {
  type Tracer = SolverEvent => Unit
  def solve(tracer: Tracer = null): ActionTreeNode
}