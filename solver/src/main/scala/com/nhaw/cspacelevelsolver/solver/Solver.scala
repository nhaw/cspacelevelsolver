package com.nhaw.cspacelevelsolver.solver

import com.nhaw.cspacelevelsolver.puzzle._

import collection._

abstract class ISolver(puzzle: Puzzle) {
  def solve(): ActionTreeNode
}

case class Solver(puzzle: Puzzle) extends ISolver(puzzle) {
  assert(puzzle.size >= 2, "Puzzle must have 2 or more nodes")
  assert(puzzle.players.size == 1, "Only single-player puzzles can be solved")
  puzzle.assertValid()

  def solve() = {
    ActionTreeNode(RootAction(), _solve(puzzle.startNode.get, puzzle.initialState))
  }

  def _solve(n: Node, st: PuzzleState): immutable.Seq[ActionTreeNode] = {
    n match {
      case n: EndNode => {
        immutable.Seq[ActionTreeNode](ActionTreeNode(EndAction(), immutable.Seq[ActionTreeNode]()))
      }
      case n => {
        val newInventory = st.inventory ++ n.contents
        val newState = PuzzleState(newInventory, st.world)
        n.to.flatMap { to =>
          val tp = new TransitionPossibilities(n, to, newInventory, st.world)
          assert(tp.entryProblems.nonEmpty || tp.validActions.nonEmpty,
                 "Either there were entry problems or there is at least the valid action of doing nothing")

          // Recursively solve given valid options
          tp.validActions.flatMap { act =>
            val subSolutions = _solve(to, act.resultState(newState))
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
