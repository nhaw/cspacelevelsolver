/**
 * Created by nhaw on 7/4/2015.
 */

package com.nhaw.cspacelevelsolver.puzzle.test

import com.nhaw.cspacelevelsolver.color._
import com.nhaw.cspacelevelsolver.puzzle._
import com.nhaw.cspacelevelsolver.solver._
import org.scalatest._

import scala.language.implicitConversions

class SolverTest extends FunSpec with Matchers with GivenWhenThen {
  val laser = Entity.LASER(Color.BLUE)
  val player = Entity.PLAYER(Color.GREEN)


  describe("Solver") {
    describe("a trivial com.nhaw.cspacelevelsolver.color.puzzle with pre-satisfied requirements") {
      val player = Entity.PLAYER(Color.GREEN)
      val definition = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >>
        NodeBuilder().setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = Solver(puzzle).solve()
      it("can be solved with one solution") {
        assert(solutionTree.solutions === 1)
      }
    }

    describe("a simple com.nhaw.cspacelevelsolver.color.puzzle with two paths and pre-satisfied requirements") {
      val player = Entity.PLAYER(Color.GREEN)
      val endNB = NodeBuilder().setEnd()
      val definition = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart()
      definition >* Seq(NodeBuilder(), // fork
        NodeBuilder()) >- endNB
      // Not allowed (same node 2x)
      //definition >* Seq(endNB, // fork to end 0
      //                  endNB) // fork to end 1
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = Solver(puzzle).solve()
      it("can be solved with two solutions") {
        assert(solutionTree.solutions === 2)
      }
    }

    describe("a simple com.nhaw.cspacelevelsolver.color.puzzle with a fork and a join and pre-satisfied requirements") {
      val player = Entity.PLAYER(Color.GREEN)
      val endNB = NodeBuilder().setEnd()
      val definition = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart()
      definition >* NodeBuilderSequence(Seq(NodeBuilder(),
        NodeBuilder())) >- endNB
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = Solver(puzzle).solve()
      it("can be solved with two solutions") {
        assert(solutionTree.solutions === 2)
      }
    }

    describe("a simple-ish com.nhaw.cspacelevelsolver.color.puzzle with two required actions to solve") {
      val player = Entity.PLAYER(Color.GREEN)
      val definition = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >>
        ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >> Color.RED >>
        (ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: ReqNoInteraction(player, Entity.BLOCKER(Color.GREEN)) :: Nil) >>
        ReqInteraction(player, Entity.PLATFORM(Color.RED)) >> Color.GREEN >>
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = Solver(puzzle).solve()
      it("can be solved with one solution") {
        assert(solutionTree.solutions === 1)
      }
      it("solution tree can be displayed") {
        solutionTree.display(Console.out)
      }

    }
  }
}