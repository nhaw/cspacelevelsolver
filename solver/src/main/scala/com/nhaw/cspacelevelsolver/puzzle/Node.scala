/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color.Color

import collection._

object Node {

  /**
   * Walk all nodes once starting from a given node recursively in no particular order
   * @param n Any node in the level
   * @param fn Callback. Must return true to continue or false to terminate walk immediately
   * @return
   */
  def walk(n: Node, fn: Node=>Boolean): Boolean = {
    val visited = mutable.HashSet[Node]()
    _walk(n, fn, visited)
  }
  private[this] def _walk(n: Node, fn: Node=>Boolean, visited: mutable.HashSet[Node]): Boolean = {
    if (visited contains n) {
      // Already hit
    } else {
      visited.add(n)
      if (!fn(n)) return false
      if (!n.from.forall(_walk(_, fn, visited))) return false
      if (!n.to.forall(_walk(_, fn, visited))) return false
    }
    true
  }

  def findStart(n:Node): Option[StartNode] = {
    var s:Option[StartNode] = None
    walk(n, _ match {
      case x: StartNode => s = Some(x); false
      case _ => true
    })
    s
  }

  def findEnds(n:Node): immutable.List[EndNode] = {
    val endNodes = mutable.ListBuffer[EndNode]()
    walk(n, _ match {
      case x: EndNode => endNodes += x; true
      case _ => true
    })
    endNodes.toList
  }

  def print(n:Node) { walk(n, x => {println(x); true}) }

  def build(nb:NodeBuilder) = { nb.make }
}

class Node(nb: NodeBuilder) {
  NodeBuilder.nodes(nb) = this; // Prevent cycles

  val id = nb.id
  val from: Seq[Node] = nb.from.map(_.make)
  val to: Seq[Node] = nb.to.map(_.make)
  val reqs: Seq[Requirement] = nb.reqs
  val contents = nb.contents.toList

  def nodeType: String = "normal"

  override def toString = {
    def listify(x:Seq[Node]) = x.map(_.id).mkString("[",",","]")
    val froms = listify(from)
    val tos = listify(to)
    val reqrepr = reqs.mkString("["," & ","]")
    val contentsrepr = if (contents.isEmpty) "none" else contents.mkString("[",",","]")
    s"<$nodeType:$id from:$froms to:$tos reqs:$reqs contents:$contentsrepr>"
  }

  //def accessible(implicit playerColor:Color, objectColor:Color) = true

  override def finalize() {
    NodeBuilder.nodes -= nb
    super.finalize()
  }
}


class StartNode(nb: NodeBuilder) extends Node(nb) {
  override def nodeType = "start"
}
class EndNode(nb: NodeBuilder) extends Node(nb) {
  override def nodeType = "end"
}

case class PuzzleState(inventory:immutable.Seq[Color], world:Requirement.EntityStates)

object Puzzle {
  def apply(anyNode: Node, name: String) = new Puzzle(anyNode, name)
}
class Puzzle(anyNode: Node, val name: String) {
  val startNode = Node.findStart(anyNode)
  val endNodes = Node.findEnds(anyNode)
  val valid = startNode.isDefined && endNodes.nonEmpty
  val (entities: immutable.List[Entity], size) = {
    var entitiesSet = immutable.Set[Entity]()
    var nodeCount = 0
    Node.walk(anyNode,
      n => {
        nodeCount+=1
        n.reqs.foreach(_.subjects.foreach(entitiesSet += _))
        true
      } )
    (entitiesSet.toList, nodeCount)
  }
  val players = entities.collect{case (e: Player) => e.asInstanceOf[Player]}.toSet
  val initialInventory: immutable.Seq[Color] = immutable.Seq[Color]()
  val initialColors: Requirement.EntityStates = entities.map{e => e -> e.initialState}.toMap
  val initialState: PuzzleState = new PuzzleState(initialInventory, initialColors)

  def assertValid() {
    if (!startNode.isDefined) throw new IllegalArgumentException("No start node found")
    if (endNodes.isEmpty) throw new IllegalArgumentException("No end nodes found")
    assert(valid, "Invalid for unlisted reason")
  }

  override def toString = s"Puzzle(nodes:$size ents:${entities.size} players:${players.size}})"
}

object Printer extends App {
  override def main(args: Array[String]) {
    println("Running")
  }
}