/**
 * Created by nhaw on 7/4/2015.
 */

package com.nhaw.cspacelevelsolver.puzzle.test

import java.io.{FileOutputStream, PrintStream, File, ByteArrayOutputStream}

import com.nhaw.cspacelevelsolver.color._
import com.nhaw.cspacelevelsolver.puzzle._
import com.nhaw.cspacelevelsolver.solver._

import org.scalatest._

import scala.sys.process._
import scala.language.implicitConversions

class ActionTest extends FunSpec with Matchers {
  describe("ColorChangeAction") {
    describe("two distinct but identical actions") {
      val platform = Entity.PLATFORM(Color.RED)
      val cca1 = ColorChangeAction(Color.GREEN, platform)
      val cca2 = ColorChangeAction(Color.GREEN, platform)
      it("hash to the same value"){
        assert(cca1.hashCode === cca2.hashCode)
      }
      it("are equal"){
        assert(cca1 equals cca2)
      }
      it("are ==="){
        assert(cca1 === cca2)
      }
      it("are =="){
        assert(cca1 == cca2)
      }
      it("will be detected as redundant in a HashSet[Action]"){
        val set = collection.immutable.HashSet[Action](cca1,cca2)
        assert(set.size === 1)
      }
    }
  }
  describe("CompoundColorChangeAction") {
    it("a trivial one hashes to a number") {
      println("CompoundColorChangeAction hashcode = " + CompoundColorChangeAction().hashCode)
    }
    it("a trivial one's hash matches another trivial one's") {
      assert(CompoundColorChangeAction().hashCode === CompoundColorChangeAction().hashCode)
    }
    it("a trivial one == another trivial one") {
      assert(CompoundColorChangeAction() === CompoundColorChangeAction())
    }
    it("a trivial one equals another trivial one") {
      assert(CompoundColorChangeAction() equals CompoundColorChangeAction())
    }
    it("a nontrivial one equals another nontrivial one") {
      val platform = Entity.PLATFORM(Color.RED)
      val ccca1 = CompoundColorChangeAction() + ColorChangeAction(Color.GREEN,platform)
      val ccca2 = CompoundColorChangeAction() + ColorChangeAction(Color.GREEN,platform)
      assert(ccca1 === ccca2)
    }
    it("a nontrivial one that operates on the same entity but with differnt colors should not match another nontrivial") {
      val platform = Entity.PLATFORM(Color.RED)
      val ccca1 = CompoundColorChangeAction() + ColorChangeAction(Color.GREEN,platform)
      val ccca2 = CompoundColorChangeAction() + ColorChangeAction(Color.BLUE,platform)
      assert(ccca1 !== ccca2)
    }
    it("a nontrivial one that looks similar to another but does not match entities does not equal another nontrivial one") {
      val ccca1 = CompoundColorChangeAction() + ColorChangeAction(Color.GREEN,Entity.PLATFORM(Color.RED))
      val ccca2 = CompoundColorChangeAction() + ColorChangeAction(Color.GREEN,Entity.PLATFORM(Color.RED))
      assert(ccca1 !== ccca2)
    }

    describe("a multi-action one") {
      val platformA = Entity.PLATFORM(Color.RED)
      val platformB = Entity.PLATFORM(Color.RED)
      val ccca1 = CompoundColorChangeAction() + ColorChangeAction(Color.GREEN, platformA) + ColorChangeAction(Color.GREEN, platformB)
      val ccca2 = CompoundColorChangeAction() + ColorChangeAction(Color.GREEN, platformA) + ColorChangeAction(Color.GREEN, platformB)

      it("equals another multi-action one") {
        assert(ccca1 === ccca2)
      }
      it("hashes the same as another multi-action one") {
        assert(ccca1.hashCode === ccca2.hashCode)
      }
      it("will be detected as redundant in a HashSet[Action]"){
        val set = collection.immutable.HashSet[Action](ccca1,ccca2)
        assert(set.size === 1)
      }
    }
  }
}

class SolverTest(factory: Puzzle => Solver) extends FunSpec with Matchers with GivenWhenThen {
  val laser = Entity.LASER(Color.BLUE)
  val player = Entity.PLAYER(Color.GREEN)

  describe(s"${this.getClass.getName}") {
    describe("a trivial puzzle with pre-satisfied requirements") {
      println("TRIVIAL PUZZLE")
      val player = Entity.PLAYER(Color.GREEN)
      val definition = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >>
        NodeBuilder().setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = factory(puzzle).solve()
      it("can be solved with one solution") {
        assert(solutionTree.numSolutions === 1)
      }
      println("DONE A")
    }

    describe("a puzzle using the same nodebuilder twice") {
      val player = Entity.PLAYER(Color.GREEN)
      val endNB = NodeBuilder().setEnd()
      it("will fail") {
        intercept[IllegalPuzzleStructure] {
          NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >* Seq(endNB, // fork to end 0
            endNB) // fork to end 1
        }
      }
      println("DONE B")
    }

    describe("a simple puzzle with two paths and pre-satisfied requirements") {
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
      val solutionTree = factory(puzzle).solve()
      it("can be solved with two solutions") {
        assert(solutionTree.numSolutions === 2)
      }

      println("DONE C")
    }

    describe("a simple puzzle with a fork and a join and pre-satisfied requirements") {
      val player = Entity.PLAYER(Color.GREEN)
      val endNB = NodeBuilder().setEnd()
      val definition = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart()
      definition >* NodeBuilderSequence(Seq(NodeBuilder(),
                                        NodeBuilder())    ) >- endNB
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = factory(puzzle).solve()
      it("can be solved with two solutions") {
        assert(solutionTree.numSolutions === 2)
      }

      println("DONE D")
    }

    describe("a simple-ish puzzle with two required actions to solve") {
      val player = Entity.PLAYER(Color.GREEN)
      val definition = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >>
        ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >> Color.RED >>
        (ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: ReqNoInteraction(player, Entity.BLOCKER(Color.GREEN)) :: Nil) >>
        ReqInteraction(player, Entity.PLATFORM(Color.RED)) >> Color.GREEN >>
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = factory(puzzle).solve()
      it("can be solved with one solution") {
        assert(solutionTree.numSolutions === 1)
      }
      it("solution tree can be displayed") {
        Console.withOut(new ByteArrayOutputStream()) {
          solutionTree.display(Console.out)
        }
      }

      println("DONE E")
    }

    describe("a puzzle with a starting requirement that is not satisfied") {
      val player = Entity.PLAYER(Color.GREEN)
      val definition =
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.RED))).setStart() >> NodeBuilder().setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = factory(puzzle).solve()
      it("cannot be solved") {
        assert(solutionTree.numSolutions === 0)
      }

      println("DONE F")
    }

    describe("a puzzle requiring two simultaneous actions to proceed between nodes") {
      val player = Entity.PLAYER(Color.GREEN)
      val definition =
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >> Color.GREEN >> Color.GREEN >>
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: Nil).setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TwoActionPuzzle")
      def tracer(evt: SolverEvent) {
        evt match {
          case NodeReached(node, depth) =>
            println(s"TwoActionPuzzle solver reached: $node ($depth)")
          case PuzzleSolved =>
            println(s"TwoActionPuzzle solver          SOLVED!")
          case DeadEndReached =>
            println(s"TwoActionPuzzle solver          DEAD END")
          case _ =>
        }
      }
      val solutionTree = factory(puzzle).solve()
      it("can be solved with one solution") {
        assert(solutionTree.numSolutions === 1)
      }
      it("solution tree can be displayed") {
        //Console.withOut(new ByteArrayOutputStream()) {
          Console.out.println("2-solution puzzle solution")
          solutionTree.display(Console.out)
        //}
      }

      println("DONE G")
    }

    describe("a simple-ish puzzle with backtracking") {
      val player = Entity.PLAYER(Color.GREEN)
      val forkA = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))) >> Color.BLUE
      val forkB = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.BLUE))).setEnd()
      val definition =
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() <<>* (forkA :: forkB :: Nil)
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "BackTrackingPuzzle")
      def tracer(evt: SolverEvent) {
        evt match {
          case NodeReached(node, depth) =>
            Console.out.println(s"BackTrack puzzle solver reached: $node ($depth)")
          case PuzzleSolved =>
            Console.out.println(s"BackTrack puzzle solver          SOLVED!")
          case DeadEndReached =>
            Console.out.println(s"BackTrack puzzle solver          DEAD END")
          case InventoryPickup(color) =>
            Console.out.println(s"BackTrack puzzle solver pickup:  $color")
          case TransitionImpossible(node, reason) =>
            Console.out.println(s"BackTrack puzzle solver no transition to: $node $reason")
        }
      }
      val solutionTree = factory(puzzle).solve(tracer)
      it("can be solved with one solution") {
        assert(solutionTree.numSolutions === 1)
      }
      val dotfname = File.createTempFile("foo",".dot")
      val imgfname = File.createTempFile("foo",".png")
      it(s"can be displayed as a graph: $imgfname") {
        puzzle.writeDot(new PrintStream(new FileOutputStream(dotfname)))
        assert(s"dot -Tpng ${dotfname.toString} -o ${imgfname.toString}".! == 0, "dot command failed")
      }
      it("solution tree can be displayed") {
        //Console.withOut(new ByteArrayOutputStream()) {
        Console.out.println("Displaying solutions to backtracking puzzle")
          solutionTree.display(Console.out)
        Console.out.println("--DONE--")
        //}
      }

      println("DONE H")
    }

    /*
    describe("a simple-ish puzzle with backtracking using the <<>* operator") {
      val player = Entity.PLAYER(Color.GREEN)
      val forkA = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))) >> Color.BLUE
      val forkB = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.BLUE))).setEnd()
      val definition =
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() <<>* Seq(forkA, forkB)
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      val solutionTree = factory(puzzle).solve()
      it("can be solved with one solution") {
        assert(solutionTree.numSolutions === 1)
      }
      it("solution tree can be displayed") {
        Console.withOut(new ByteArrayOutputStream()) {
          solutionTree.display(Console.out)
        }
      }

      println("DONE I")
    }

    describe("a puzzle with no player cannot be solved") {
      val definition = NodeBuilder().setStart() >> NodeBuilder().setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      intercept[AssertionError] {
        factory(puzzle).solve()
      }

      println("DONE J")
    }

    describe("a puzzle where a while-colored player can walk over everything (except black)") {
      // also checks that the solver can pass through all colors
      val player = Entity.PLAYER(Color.WHITE)
      val definition =
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.BLUE))).setStart() >>
          ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >>
          ReqInteraction(player, Entity.PLATFORM(Color.VIOLET)) >>
          ReqInteraction(player, Entity.PLATFORM(Color.WHITE)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.BLACK)) >>
          ReqInteraction(player, Entity.PLATFORM(Color.CYAN)) >>
          ReqInteraction(player, Entity.PLATFORM(Color.METAL)) >>
          ReqInteraction(player, Entity.PLATFORM(Color.RED)) >>
          ReqInteraction(player, Entity.PLATFORM(Color.YELLOW)) >>
          NodeBuilder().setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "TrivialPuzzle")
      var nodesReached, solutions, deadEnds: Int = 0
      val tracer = CountingTracer()
      val solutionTree = factory(puzzle).solve(tracer)
      it("tracer reports the right number of solutions, nodes, and dead-ends") {
        assert(tracer.nodesReached === 9)
        assert(tracer.solutions === 1)
        assert(tracer.deadEnds === 0)
      }
      it("can be solved with one solution") {
        assert(solutionTree.numSolutions === 1)
      }
      it("solution tree can be displayed") {
        Console.withOut(new ByteArrayOutputStream()) {
          solutionTree.display(Console.out)
        }
      }

      println("DONE K")
    }

    describe("a puzzle with a few solutions, and superfluous items") {
      // also checks that the solver can pass through all colors
      val player = Entity.PLAYER(Color.BLUE)
      val definition =
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.BLUE))).setStart() >> Color.RED >> Color.GREEN >> Color.YELLOW >>
        ReqInteraction(player, Entity.PLATFORM(Color.YELLOW)) >>
        NodeBuilder().setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "ManyOptionsPuzzle")
      var maxDepth: (Node,Int) = (null, 0)
      def tracer(evt: SolverEvent) {
        evt match {
          case NodeReached(node, depth) =>
            if (depth > maxDepth._2) maxDepth = (node, depth)
          //println(s"long puzzle solver reached: $node ($depth)")
          case PuzzleSolved =>
          //println(s"long puzzle solver          SOLVED!")
          case DeadEndReached =>
          //println(s"long puzzle solver          DEAD END")
        }
      }
      val solutionTree = factory(puzzle).solve(tracer)
      it("can be solved with a specific number of solutions") {
        assert(solutionTree.numSolutions == 3)
      }
      it("solution tree can be displayed") {
        Console.withOut(new ByteArrayOutputStream()) {
          solutionTree.display(Console.out)
        }
      }
      println("DONE L")
    }

    describe("a puzzle with lots of nodes and items that should produce many of possible outcomes") {
      // also checks that the solver can pass through all colors
      val player = Entity.PLAYER(Color.BLUE)
      val definition =
        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.BLUE))).setStart() >> Color.all >>
          ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.VIOLET)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.WHITE)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.BLACK)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.CYAN)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.METAL)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.RED)) >>
          //ReqInteraction(player, Entity.PLATFORM(Color.YELLOW)) >>
          NodeBuilder().setEnd()
      val topo = Node.build(definition)
      val puzzle = Puzzle(topo, "ManyOptionsPuzzle")
      var maxDepth: (Node,Int) = (null, 0)
      def tracer(evt: SolverEvent) {
        evt match {
          case NodeReached(node, depth) =>
            if (depth > maxDepth._2) maxDepth = (node, depth)
          //println(s"long puzzle solver reached: $node ($depth)")
          case PuzzleSolved =>
          //println(s"long puzzle solver          SOLVED!")
          case DeadEndReached =>
          //println(s"long puzzle solver          DEAD END")
        }
      }
      val solutionTree = factory(puzzle).solve(tracer)
      println(s"open-ended puzzle had ${solutionTree.numSolutions} solutions with a max depth of ${maxDepth._2}")
      it("can be solved with a specific number of solutions") {
        assert(solutionTree.numSolutions == 2217)
      }
      it("solution tree can be displayed") {
        Console.withOut(new ByteArrayOutputStream()) {
          solutionTree.display(Console.out)
        }
      }
      println("DONE M")
    }

    /**
     * This test only runs with at least 4G of memory when first tested - probably more now. It has almost 3 million
     * and isn't necessary now. When a distributed solver is working, it may be.
     */
//    describe("an extreme scale-testing puzzle with lots of nodes and items that should produce an absurd amount of possible outcomes") {
//      // also checks that the solver can pass through all colors
//      val player = Entity.PLAYER(Color.BLUE)
//      val definition =
//        NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.BLUE))).setStart() >> Color.all >>
//        ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >> Color.all >>
//        ReqInteraction(player, Entity.PLATFORM(Color.VIOLET)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.WHITE)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.BLACK)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.CYAN)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.METAL)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.RED)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.YELLOW)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.VIOLET)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.WHITE)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.BLACK)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.CYAN)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.METAL)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.RED)) >>
//        ReqInteraction(player, Entity.PLATFORM(Color.YELLOW)) >>
//        NodeBuilder().setEnd()
//      val topo = Node.build(definition)
//      val puzzle = Puzzle(topo, "ManyOptionsPuzzle")
//      var maxDepth: (Node,Int) = (null, 0)
//      var numSolutions = 0
//      //def tracer(evt: SolverEvent) {}
//      def tracer(evt: SolverEvent) {
//        evt match {
//          case NodeReached(node, depth) =>
//            if (depth > maxDepth._2) maxDepth = (node, depth)
//            //println(s"long puzzle solver reached: $node ($depth)")
//          case PuzzleSolved() =>
//            numSolutions += 1
//            if (numSolutions % 10000 == 0) println(s"$numSolutions for scale-test puzzle")
//            //println(s"long puzzle solver          SOLVED!")
//          case DeadEndReached =>
//            //println(s"long puzzle solver          DEAD END")
//        }
//      }
//      val solutionTree = factory(puzzle).solve(tracer)
//      println(s"long, crazy puzzle had ${solutionTree.numSolutions} solutions with a max depth of ${maxDepth._2}")
//      it("can be solved with multiple solutions") {
//        assert(solutionTree.numSolutions === 2704323)
//      }
//    }*/
  }
}
