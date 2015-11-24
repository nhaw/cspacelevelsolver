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
  val test1bld = NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setStart() >>
                 ReqInteraction(player, Entity.PLATFORM(Color.GREEN)) >> Color.RED >>
                 (ReqInteraction(player, Entity.PLATFORM(Color.RED)) :: ReqNoInteraction(player, Entity.BLOCKER(Color.GREEN)) :: Nil ) >>
                 ReqInteraction(player, Entity.PLATFORM(Color.RED)) >> Color.GREEN >>
                 NodeBuilder(ReqInteraction(player, Entity.PLATFORM(Color.GREEN))).setEnd()
  val test1 = Node.build(test1bld)

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
    it("can be printed") {
      Node.print(test1)
    }
  }

  describe("A simple puzzle") {
    Then("can be constructed")
    val puzzle = Puzzle(test1, "SimplePuzzle")
    Then("can be printed")
    println(puzzle)
    it("has the right number of entities") {
      assert(puzzle.entities.size === 7)
    }
  }
}