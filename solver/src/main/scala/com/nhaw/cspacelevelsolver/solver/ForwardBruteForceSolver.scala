package com.nhaw.cspacelevelsolver.solver

import com.nhaw.cspacelevelsolver.color.Color
import com.nhaw.cspacelevelsolver.puzzle._
import collection._

case class ForwardBruteForceSolver(puzzle: Puzzle) extends Solver(puzzle) {
  assert(puzzle.size >= 2, s"Puzzle must have 2 or more nodes: ${Node.print(puzzle.startNode.get)}")
  assert(puzzle.players.size == 1, "Only single-player puzzles can be solved")
  assert(puzzle.switches.isEmpty, "Cannot solve puzzles with switches")
  puzzle.assertValid()

  def solve(tracer: Tracer): ActionTreeNode = {
    // Check that this puzzle can be started
    val startNodeLink = puzzle.startNodeLink.get
    val tp = new TransitionPossibilities(startNodeLink, immutable.Seq[Color](), puzzle.initialState.world)
    if (tp.isDeadEnd) {
      if (tracer != null) { tracer(DeadEndReached) } // dead end before first node
      ActionTreeNode(RootAction(), startNodeLink.dest, immutable.Seq[ActionTreeNode]())
    } else {
      ActionTreeNode(RootAction(), startNodeLink.dest, _solve(tracer, 0, startNodeLink.dest, puzzle.initialState, immutable.HashSet[Node]()))
    }
  }

  /**
   * @param tracer Interface through which events are to be emitted while solving the puzzle
   * @param depth Current number of node-traversals (levels of recursion) from starting node (0)
   * @param n Node from which the puzzle is being solved
   * @param st Puzzle state for handling this node
   * @param offLimits Set of nodes visited to reach the current node n. This is solely for preventing infinite cyclical
   *                backtracking. This set is empty at the start of the puzzle or after a color is picked up, switch
   *                thrown, or other change in state that allows the player to make progress that was otherwise not
   *                possible. Color-changing actions are not included in this list because the player could have
   *                performed them at any point. Color pickups (which allow new color changes), do clear this set
   * @return ActionTreeNode that is the root of a tree of possible solutions
   */
  private[this] def _solve(tracer: Tracer, depth: Int, n: Node, st: PuzzleState, offLimits: immutable.HashSet[Node]): immutable.Seq[ActionTreeNode] = {
    def trace(event: SolverEvent) = if (tracer != null) { tracer(event) }

    trace(NodeReached(n, depth))
    n match {
      case n: EndNode => {
        if (tracer != null) { tracer(PuzzleSolved)}
        immutable.Seq[ActionTreeNode](ActionTreeNode(EndAction(), n, immutable.Seq[ActionTreeNode]()))
      }
      case n => {
        assume(n.switches.isEmpty, "Puzzle was thought to contain no switches")

        // Pick up items
        // TODO: Allow selective item pickup if inventory is limited in size
        val newInventory = st.inventory ++ n.contents
        n.contents.foreach(item => trace(InventoryPickup(item)))

        // History is erased for the purpose of further recursion once something is picked up
        val newOffLimits = { if (n.contents.isEmpty) offLimits
                             else immutable.HashSet[Node]()
                           } + n
        // TODO: Also reset history on actions where switches are thrown or world events take place

        val newState = PuzzleState(newInventory, st.world)

        val possibleNextNodes = n.to.filter{nl => !newOffLimits.contains(nl.dest)} // reject nodes already in histories to prevent cycles
        val tps = possibleNextNodes.map { link =>
          val tp = new TransitionPossibilities(link, newInventory, st.world)
          assert(tp.entryProblems.nonEmpty || tp.validActions.nonEmpty,
            "Either there were entry problems or there is at least the valid action of doing nothing")
          (tp,link)
        }

        // See if node is passable and which targets are not and update tracer
        if (tps.count{ tp =>
          if (tp._1.isPassable) {
            true
          } else {
            trace(TransitionImpossible(tp._2, ""))
            false
          }
        } == 0) { trace(DeadEndReached) }

        val actions =
          tps.flatMap { case (tp: TransitionPossibilities, link:NodeLink) =>
            // Recursively solve
            tp.validActions.flatMap { act =>
              trace(LinkTraversed(link, depth))
              val subSolutions = _solve(tracer, depth+1, link.dest, act.resultState(newState), newOffLimits)
              if (subSolutions.nonEmpty)
                Seq[ActionTreeNode](ActionTreeNode(act, link.dest, subSolutions))
              else
                Seq[ActionTreeNode]()
            }
          }

        // Apply each item pickup before the recursive solutions. This is awkward due to the recursion.
        // TODO: Allow selective item pickup (see comments earlier in this function regarding this). This will make pickup-choice a sensibly-recursive action
        n.contents.foldLeft(actions){case (a,item) => Seq[ActionTreeNode](ActionTreeNode(PickupAction(item), n, a))}
      }.to[immutable.Seq]
    }
  }
}
