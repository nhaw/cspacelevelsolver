package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color.Color

import java.io.{FileOutputStream, PrintStream}
import org.clapper.argot._
import scala.sys.process._

object PuzzleList extends App {

  /**
   * Example cmdline:
   * > sbt "project solver" "run-main com.nhaw.cspacelevelsolver.puzzle.PuzzleList --puzzle-name simple_test  --dot-file foo.dot"
   */

  val parser = new ArgotParser("PuzzleList", preUsage=Some("Version 1.0")) {
    import ArgotConverters._
    val puzzleName = this.option[String]("puzzle-name", "name", "Name of the puzzle to show")
    val dotFile = this.option[String]("dot-file", "filename", "Name of the dot file to generate")
  }

  parser.parse(args)

  val puzzlePrinter = new PuzzlePrinter

  parser.puzzleName.value.foreach { puzzleName =>
    puzzlePrinter.print(puzzleName)
    parser.dotFile.value.foreach{ fname =>
      println(s"Writing dot to $fname")
      val puzzle = puzzlePrinter.puzzles(puzzleName)
      puzzle.writeDot(new PrintStream(new FileOutputStream(fname)))
      assert(s"dot -Tpng $fname -o ${fname+".png"}".! == 0, "dot command failed")
    }
  }
}

class PuzzlePrinter {

  val puzzlesRaw = collection.immutable.Map(
  "simple_test" -> {
    val player = Entity.PLAYER(Color.GREEN)
    NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >>
      ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >> Color.RED >>
      (ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: ReqNoInteraction(player, Entity.BLOCKER(Color.GREEN)) :: Nil) >>
      ReqInteraction(player, Entity.PLATFORM(Color.RED)) >> Color.GREEN >>
      NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setEnd()
  },
  "trivial" -> {
    val player = Entity.PLAYER(Color.GREEN)
    NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >>
      NodeBuilder().setEnd()
  }
  )

  val puzzles = puzzlesRaw.map(x => x._1 -> Node.build(x._2)).map(pair => pair._1 -> Puzzle(pair._2, pair._1))

  def print(puzzleName: String) {
    println(s"Printing: $puzzleName")

    println("This puzzle is")
    assert(puzzles(puzzleName).startNodeLink.get.src == null)
    Node.print(puzzles(puzzleName).startNodeLink.get.dest)
  }
}
