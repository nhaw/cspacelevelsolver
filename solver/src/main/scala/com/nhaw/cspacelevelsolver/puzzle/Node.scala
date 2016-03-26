/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import java.io.{PrintStream, OutputStream}

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

  private[this] def _walk(nl: NodeLink, fn: Node=>Boolean, visited: mutable.HashSet[Node]): Boolean = {
    _walk(nl.dest, fn, visited)
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
    var s: Option[StartNode] = None
    walk(n, { case x: StartNode => s = Some(x); false
              case _ => true
            }
        )
    s
  }

  def findEnds(n:Node): immutable.List[EndNode] = {
    val endNodes = mutable.ListBuffer[EndNode]()
    walk(n, { case x: EndNode => endNodes += x; true
              case _ => true
            }
        )
    endNodes.toList
  }

  def print(n:Node) { walk(n, x => {println(x); true}) }

  def build(nb:NodeBuilder) = { nb.make }
  def build(nbs:NodeBuilderGroup) = { assert(nbs.elements.nonEmpty); nbs.elements.head.make }
}

class Node(nb: NodeBuilder) {
  NodeBuilder.nodes(nb) = this; // Prevent cycles

  val id = nb.id
  val from: Seq[NodeLink] = nb.from.map(_.make)
  val to: Seq[NodeLink] = nb.to.map(_.make)
  val reqs: Seq[Requirement] = nb.occupyReqs
  val contents = nb.contents.toList
  val switches = nb.switches.toList

  def nodeType: String = "normal"

  override def toString = {
    def listify(x:Seq[Node]) = x.map(_.id).mkString("[",",","]")
    val froms = listify(from.map(_.src))
    val tos = listify(to.map(_.dest))
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

object Printer extends App {
  override def main(args: Array[String]) {
    println("Running")
  }
}