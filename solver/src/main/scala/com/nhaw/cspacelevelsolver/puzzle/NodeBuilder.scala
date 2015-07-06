/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color._

import java.util.concurrent.atomic.AtomicInteger

import collection._

import language.implicitConversions

case class NodeBuilderSequence(private[puzzle] val elements: Seq[NodeBuilder]) {

  /**
   * Join back to one common node
   */
  def >- (nb: NodeBuilder) = { elements.foreach(_.addTo(nb)); nb }

  def >>(oelements: Seq[NodeBuilder]): NodeBuilderSequence = {
    assert(elements.size == oelements.size, s"${elements.size} != ${oelements.size}: Number of NodeBuilders in sequence on left side of " +
                                  "operator *> does not match number of NodeBuilders in sequence on the right")
    NodeBuilderSequence(elements.zip(oelements).map{case (nbx: NodeBuilder, onbx: NodeBuilder) => { nbx.addTo(onbx); onbx} })
  }
  def >>(onbs: NodeBuilderSequence): NodeBuilderSequence = >>(onbs.elements)
}

class NodeBuilder(val reqs: Seq[Requirement]) {

  implicit def toNodeBuilderSeq(nbs: Seq[NodeBuilder]): NodeBuilderSequence = NodeBuilderSequence(nbs)

  val id = NodeBuilder.nextId.getAndIncrement()

  val from = mutable.ArrayBuffer.empty[NodeBuilder]
  val to = mutable.ArrayBuffer.empty[NodeBuilder]

  private[this] var _isStart: Boolean = false
  def isStart = _isStart
  def setStart() = {_isStart = true; this }
  private[this] var _isEnd: Boolean = false
  def isEnd = _isEnd
  def setEnd() = {_isEnd = true; this }

  // TODO: Implement bi-direcitonal traversal (typical) so that the player can go forward and backward. Would result in infinite loop now.
  // From a com.nhaw.cspacelevelsolver.solver perspective, going backward is only beneficial if new inventory has been picked up or state has been
  // changed in some way that going backward will allow access to previously closed paths. To implement this in solving,
  // forward traversal should be required in recursion until new inventory is acquired or state is otherwised changed
  // (e.g. point-reach trigger, switch thrown, etc.)
  //def <<>> (nb: NodeBuilder) = { addTo(nb); nb.addTo(this); nb }

  def >>(nb: NodeBuilder) = { addTo(nb); nb }
  def >*(nbs: Seq[NodeBuilder]): NodeBuilderSequence = { nbs.elements.foreach(addTo); nbs }
  def >*(nbs: NodeBuilderSequence): NodeBuilderSequence = { nbs.elements.foreach(addTo); nbs }
  def >>(req: Requirement) = { val nb = NodeBuilder(req); addTo(nb); nb }
  def >>(reqs: Seq[Requirement]) = { val nb = NodeBuilder(reqs); addTo(nb); nb }

  val contents = mutable.ListBuffer[Color]()
  def >>(color: Color) = { contents += color; this }

  //def >>(switch: Activator) = {}

  private[puzzle] def addTo(n: NodeBuilder) {
    assert(n != this)
    assert(!to.contains(n))
    to.append(n)
    n.addFrom(this)
  }
  private[puzzle] def addFrom(n: NodeBuilder) {
    assert(n != this)
    assert(!from.contains(n))
    from.append(n)
  }

  private[this] def createNode() = {
    if (isStart) new StartNode(this) else if (isEnd) new EndNode(this) else new Node(this)
  }

  override def toString = s"NodeBuilder(id:$id, contents:${contents.size} reqs:${reqs.size} from:${from.size} to:${to.size} start:$isStart end:$isEnd)"

  def make: Node = { NodeBuilder.nodes.getOrElse(this, createNode()) }
}

object NodeBuilder {
  val nodes = mutable.Map[NodeBuilder, Node]()
  val nextId = new AtomicInteger(0)

  def apply() = new NodeBuilder(Seq[Requirement]())
  def apply(reqs: Seq[Requirement]) = new NodeBuilder(reqs)
  def apply(req: Requirement) = new NodeBuilder(Seq[Requirement](req))
}