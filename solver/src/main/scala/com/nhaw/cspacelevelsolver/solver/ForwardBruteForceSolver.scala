package com.nhaw.cspacelevelsolver.solver

import com.nhaw.cspacelevelsolver.color.Color
import com.nhaw.cspacelevelsolver.puzzle._
import collection._

case class ForwardBruteForceSolver(puzzle: Puzzle) extends Solver(puzzle) {
  assert(puzzle.size >= 2, "Puzzle must have 2 or more nodes")
  assert(puzzle.players.size == 1, "Only single-player puzzles can be solved")
  assert(puzzle.switches.isEmpty, "Cannot solve puzzles with switches")
  puzzle.assertValid()

  def solve(tracer: Tracer): ActionTreeNode = {
    // Check that this puzzle can be started
    val tp = new TransitionPossibilities(null, puzzle.startNode.get, immutable.Seq[Color](), puzzle.initialState.world)
    if (tp.isDeadEnd) {
      if (tracer != null) { tracer(DeadEndReached()) } // dead end before first node
      ActionTreeNode(RootAction(), immutable.Seq[ActionTreeNode]())
    } else {
      ActionTreeNode(RootAction(), _solve(tracer, 0, puzzle.startNode.get, puzzle.initialState, immutable.HashSet[Node]()))
    }
  }

  /**
   * @param tracer Interface through which events are to be emitted while solving the puzzle
   * @param depth Current number of node-traversals (levels of recursion) from starting node (0)
   * @param n Node being inspected
   * @param st Puzzle state for handling this node
   * @param offLimits Set of nodes visited to reach the current node n. This is solely for preventing infinite cyclical
   *                backtracking. This set is empty at the start of the puzzle or after a color is picked up, switch
   *                thrown, or other change in state that allows the player to make progress that was otherwise not
   *                possible. Color-changing actions are not included in this list because the player could have
   *                performed them at any point. Color pickups (which allow new color changes), do clear this set
   * @return ActionTreeNode that is the root of a tree of possible solutions
   */
  private[this] def _solve(tracer: Tracer, depth: Int, n: Node, st: PuzzleState, offLimits: immutable.HashSet[Node]): immutable.Seq[ActionTreeNode] = {
    if (tracer != null) { tracer(NodeReached(n, depth))}
    n match {
      case n: EndNode => {
        if (tracer != null) { tracer(PuzzleSolved())}
        immutable.Seq[ActionTreeNode](ActionTreeNode(EndAction(), immutable.Seq[ActionTreeNode]()))
      }
      case n => {
        assume(n.switches.isEmpty, "Puzzle was thought to contain no switches")

        // Pick up items
        val newInventory = st.inventory ++ n.contents

        // History is erased for the purpose of further recursion once something is picked up
        val newOffLimits = { if (n.contents.isEmpty) offLimits
                             else immutable.HashSet[Node]()
                           } + n

        val newState = PuzzleState(newInventory, st.world)

        // TODO: reset history on actions where switches are thrown or world events take place

        val possibleNextNodes = n.to.filter(!offLimits.contains(_)) // reject nodes already in histories to prevent cycles
        val tps = possibleNextNodes.map { to =>
          val tp = new TransitionPossibilities(n, to, newInventory, st.world)
          assert(tp.entryProblems.nonEmpty || tp.validActions.nonEmpty,
            "Either there were entry problems or there is at least the valid action of doing nothing")
          (tp,to)
        }

        // See if node is passable and which targets are not and update tracer
        if (tps.count{ tp =>
          if (tp._1.isPassable) {
            true
          } else {
            NoTransitionPossible(tp._2)
            false
          }
        } == 0) { if (tracer != null) { tracer(DeadEndReached()) } }

        tps.flatMap { case (tp: TransitionPossibilities, to:Node) =>
          // Recursively solve
          tp.validActions.flatMap { act =>
            val subSolutions = _solve(tracer, depth+1, to, act.resultState(newState), newOffLimits)
            if (subSolutions.nonEmpty)
              Seq[ActionTreeNode](ActionTreeNode(act, subSolutions))
            else
              Seq[ActionTreeNode]()
          }
        }.to[immutable.Seq]
      }
    }
  }
}
