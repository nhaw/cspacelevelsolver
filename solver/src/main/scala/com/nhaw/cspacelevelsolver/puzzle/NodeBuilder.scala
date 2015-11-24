/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color._
import java.util.concurrent.atomic.AtomicInteger

import collection._
import language.implicitConversions

class IllegalPuzzleStructure(what: String) extends RuntimeException(what)

case class NodeBuilderSequence(elements: Seq[NodeBuilder]) {

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

  val id = NodeBuilder.nextId.getAndIncrement()

  val from = mutable.ArrayBuffer.empty[NodeBuilder]
  val to = mutable.ArrayBuffer.empty[NodeBuilder]

  val contents = mutable.ListBuffer[Color]()
  val switches = mutable.ListBuffer[Switch]()

  private[this] var _isStart: Boolean = false
  def isStart = _isStart
  def setStart() = {_isStart = true; this }
  private[this] var _isEnd: Boolean = false
  def isEnd = _isEnd
  def setEnd() = {_isEnd = true; this }

  // Note that from a com.nhaw.cspacelevelsolver.solver perspective, going backward is only beneficial if new inventory
  // has been picked up or state has been changed in some way that going backward will allow access to previously closed
  // paths. To implement this in solving, forward traversal should be required in recursion until new inventory is
  // acquired or state is otherwise changed (e.g. point-reach trigger, switch thrown, etc.).
  def <<>> (nb: NodeBuilder) = { addTo(nb); nb.addTo(this); nb }
  def <<>* (nbs: NodeBuilderSequence) = { nbs.elements.foreach { nb => addTo(nb); nb.addTo(this); }; nbs }

  def >>(nb: NodeBuilder) = { addTo(nb); nb }
  def >*(nbs: Seq[NodeBuilder]): NodeBuilderSequence = { nbs.elements.foreach(addTo); nbs }
  def >*(nbs: NodeBuilderSequence): NodeBuilderSequence = { nbs.elements.foreach(addTo); nbs }
  def >>(req: Requirement) = { val nb = NodeBuilder(req); addTo(nb); nb }
  def >>(reqs: Seq[Requirement]) = { val nb = NodeBuilder(reqs); addTo(nb); nb }

  def >>(color: Color) = { contents += color; this }
  def >>(switch: Switch) = { switches += switch; this }

  class RequirementSeq(val seq: Seq[Requirement])
  implicit def toRequirementSeq(seq: Seq[Requirement]):RequirementSeq = new RequirementSeq(seq)

  def apply: PartialFunction[Any,NodeBuilder] = {
    case nb: NodeBuilder => this >> nb
    case req: Requirement => this >> req
    case reqs: RequirementSeq => this >> reqs.seq
    case color: Color => this >> color
    case switch: Switch => this >> switch
    case other => throw new IllegalArgumentException(s"Does not know how to apply a ${other} or this type wouldn't produce a single NodeBuilder")
  }

  def >>[T](iter: Iterable[T]) = {
    iter.foldLeft[NodeBuilder](this)(_.apply(_))
    this
  }

  //def >>(switch: Activator) = {}

  private[puzzle] def addTo(n: NodeBuilder) {
    assert(n != this)
    if (to.contains(n)) { throw new IllegalPuzzleStructure(s"Node ${this.toString} already points to ${to.toString}") }
    to.append(n)
    n.addFrom(this)
  }
  private[puzzle] def addFrom(n: NodeBuilder) {
    assert(n != this)
    if (from.contains(n)) { throw new IllegalPuzzleStructure(s"Node ${this.toString} already points to ${from.toString}") }
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

  implicit def seqToNodeBuilderSeq(nbs: Seq[NodeBuilder]): NodeBuilderSequence = NodeBuilderSequence(nbs)
  implicit def listToNodeBuilderSeq(nbs: List[NodeBuilder]): NodeBuilderSequence = NodeBuilderSequence(nbs)
}