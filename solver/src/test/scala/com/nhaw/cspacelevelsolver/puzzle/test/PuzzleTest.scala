/**
 * Created by nhaw on 7/4/2015.
 */

package com.nhaw.cspacelevelsolver.puzzle.test

import com.nhaw.cspacelevelsolver.color._
import com.nhaw.cspacelevelsolver.puzzle._
import com.nhaw.cspacelevelsolver.solver._
import org.scalatest._

import scala.language.implicitConversions

class PuzzleTest extends FunSpec with Matchers with GivenWhenThen {
  val laser = Entity.LASER(Color.BLUE)
  val player = Entity.PLAYER(Color.GREEN)
  val test1bldStart = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart()
  val test1bld = test1bldStart >>
                 ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >> Color.RED >>
                 (ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: ReqNoInteraction(player, Entity.BLOCKER(Color.GREEN)) :: Nil ) >>
                 ReqInteraction(player, Entity.PLATFORM(Color.RED)) >> Color.GREEN >>
                 NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setEnd()
  var test1: Node = null // Node.build(test1bld)


  println(s"TEMP: test1start: $test1bldStart")
  println(s"TEMP: test1end: $test1bld")


  val groupA = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart()
  println(s"TEMP: A: ${groupA}")

  val groupB = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))) >> Color.RED
  val groupBTotal = groupA >> groupB
  println(s"TEMP: B: ${groupBTotal}")

  val groupC = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: ReqNoInteraction(player, Entity.BLOCKER(Color.GREEN)) :: Nil)
  val groupCTotal = groupBTotal >> groupC
  println(s"TEMP: C: ${groupCTotal}")

  val groupD = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.RED))) >> Color.GREEN >>
    NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setEnd()
  val groupDTotal = groupCTotal >> groupD
  println(s"TEMP: D: ${groupDTotal}")

  describe("Colors") {
    it ("should interact reflexively") {
      Color.all.foreach(c1 => Color.all.foreach(c2 => assert((c1 ~ c2) === (c2 ~ c1), s"Color $c1 and $c2 do not interact reflexively")))
    }
  }

  describe("Entities") {
    it("can be printed") {
      println(player)
      println(laser)
    }
  }

  describe("NodeBuilder") {
    it("can be printed") {
      println(test1bld)
    }
  }

  describe("Nodes") {
    Then("can be built from NodeBuilder")
    println(s"TEMP: building node 1")
    test1 = Node.build(test1bld)
    println(s"TEMP: building node 2")

    Then("can be printed")
    Node.print(test1)
  }

  describe("A simple puzzle") {
    Then("can be constructed")
    println(s"TEMP: ${test1} f:${test1.from} t:${test1.to}")
    println(s"TEMP: creating puzzle from node")
    val puzzle = Puzzle(test1, "SimplePuzzle")

    println(s"TEMP: FROM START")
    Node.print(test1)

    println(s"TEMP: FROM END")
    Node.print(puzzle.endNodes.head)

    Then("can be printed")
    println(puzzle)

    Then("has the right number of entities")
    assert(puzzle.entities.size === 7)
  }

  describe("A puzzle with a loop") {
    Then("can be built")
    val startNode = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart()
    val endNode = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setEnd()
    val loopNode = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN)))
    startNode >> ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >> loopNode >> endNode
    loopNode >> startNode
    val testLoop = Node.build(startNode)

    Then("can be constructed")
    val puzzle = Puzzle(testLoop, "SimplePuzzle")

    Then("can be printed")
    println(puzzle)

    Then("has the right number of entities")
    assert(puzzle.entities.size === 7)
  }
}