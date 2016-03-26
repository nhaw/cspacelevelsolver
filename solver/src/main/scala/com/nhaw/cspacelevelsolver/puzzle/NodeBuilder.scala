/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color._
import java.util.concurrent.atomic.AtomicInteger

import scala.collection._
import language.implicitConversions

class IllegalPuzzleStructure(what: String) extends RuntimeException(what)

/**
  * Allow operations on sequences of NodeBuilders at once
  */
case class NodeBuilderGroup(elements: Seq[NodeBuilder]) {

  import NodeBuilder.NoReqs

  assert(elements.toSet.size == elements.size, "elements must be unique")

  /**
   * Link back to one common node (creates an identical link for each element in this sequence using the given reqs)
   */
  def >- (nb: NodeBuilder, linkReqs: Seq[Requirement] = NoReqs) = { elements.foreach(el => el.addTo(NodeLinkBuilder(el, nb, linkReqs))); nb }

  /**
    * Link all elements in this group 1-1 to elements in another group of the same size based on order
    * @note This is not expected to be very common since the same link requirements are used for each link
    */
  def >>(otherElements: Seq[NodeBuilder], reqs: Seq[Requirement] = Seq()): NodeBuilderGroup = {
    assert(elements.size == otherElements.size, s"${elements.size} != ${otherElements.size}: Number of NodeBuilders in sequence on left side of " +
                                  "operator *> does not match number of NodeBuilders in sequence on the right")
    NodeBuilderGroup(elements.zip(otherElements).map{case (nbx: NodeBuilder, onbx: NodeBuilder) => { nbx.addTo(NodeLinkBuilder(nbx, onbx, reqs)); onbx} })
  }
  def >>(onbs: NodeBuilderGroup): NodeBuilderGroup = >>(onbs.elements)

  /**
    * Link all elements in this group to all elements in another group (n*m links)
    * @note This is not expected to be very common since the same link requirements are used for each link
    */
  def >*>(otherElements: Seq[NodeBuilder], reqs: Seq[Requirement] = Seq()): NodeBuilderGroup = {
    assert(elements.size == otherElements.size, s"${elements.size} != ${otherElements.size}: Number of NodeBuilders in sequence on left side of " +
      "operator *> does not match number of NodeBuilders in sequence on the right")
    NodeBuilderGroup(elements.flatMap{nbx => otherElements.map{onbx => nbx.addTo(NodeLinkBuilder(nbx, onbx, reqs)); onbx} })
  }
  def >*>(onbs: NodeBuilderGroup): NodeBuilderGroup = >*>(onbs.elements)
}

/**
  * Connection from one node to the next and any associated requirements (e.g. walls standing in the way which cannot,
  * themselves, be occupied)
  */
case class NodeLinkBuilder(src: NodeBuilder, dest: NodeBuilder, reqs: Seq[Requirement] = Seq()) {

  override def toString() = src.toString + "->{" + reqs.mkString(",") + "}->" + dest.toString

  def make: NodeLink = {
    NodeBuilder.nodeLinks.getOrElse(this, {
      val srcNode = dest.make
      val dstNode = dest.make
      NodeLink(srcNode, dstNode, reqs)
    })
  }
}

/**
  * Describes a node
  * @param occupyReqs Requirements for persistently occupying this node and remaining within this node (e.g. player
  *                   matching the color of a platform or player not matching the color of an obstacle on the platform).
  *                   These reqs may be shared with to-links in the case where changing color causes the player to fall
  *                   to another node.
  */
class NodeBuilder(val occupyReqs: Seq[Requirement]) {

  import NodeBuilder.NoReqs

  val id = NodeBuilder.nextId.getAndIncrement()

  val from = mutable.ArrayBuffer.empty[NodeLinkBuilder]
  val to = mutable.ArrayBuffer.empty[NodeLinkBuilder]

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

  // Bidirectional links
  // Note that these functions are bidirectional but use only one set of link requirements for both directions
  def <<>> (nb: NodeBuilder, reqs: Seq[Requirement] = Seq()) = {
    addTo(NodeLinkBuilder(this, nb, reqs))
    nb.addTo(NodeLinkBuilder(nb, this, reqs))
    nb
  }
  def <<>* (nbs: NodeBuilderGroup, reqs: Seq[Requirement] = Seq()) = {
    nbs.elements.foreach { nb =>
      addTo(NodeLinkBuilder(this, nb, reqs))
      nb.addTo(NodeLinkBuilder(nb, this, reqs))
    }
    nbs
  }

  // Unidirectional links
  def >>(nb: NodeBuilder) = { addTo(NodeLinkBuilder(this, nb, NoReqs)); nb }
  def >>(nb: NodeBuilder, linkReqs: Seq[Requirement]) = { addTo(NodeLinkBuilder(this, nb, linkReqs)); nb }
  def >>(occupyReq: Requirement) = { val nb = NodeBuilder(occupyReq); addTo(NodeLinkBuilder(this, nb, NoReqs)); nb }
  def >>(occupyReq: Requirement, linkReqs: Seq[Requirement]) = { val nb = NodeBuilder(occupyReq); addTo(NodeLinkBuilder(this, nb, linkReqs)); nb }
  def >>(occupyReqs: Seq[Requirement]) = { val nb = NodeBuilder(occupyReqs); addTo(NodeLinkBuilder(this, nb, NoReqs)); nb }
  def >>(occupyReqs: Seq[Requirement], linkReqs: Seq[Requirement]) = { val nb = NodeBuilder(occupyReqs); addTo(NodeLinkBuilder(this, nb, linkReqs)); nb }
  def >*(nbs: Seq[NodeBuilder]): NodeBuilderGroup = { nbs.elements.foreach(nb => NodeLinkBuilder(this, nb, NoReqs)); nbs }
  def >*(nbs: Seq[NodeBuilder], linkReqs: Seq[Requirement]): NodeBuilderGroup = { nbs.elements.foreach(nb => NodeLinkBuilder(this, nb, linkReqs)); nbs }
  def >*(nbs: NodeBuilderGroup): NodeBuilderGroup = { nbs.elements.foreach(nb => NodeLinkBuilder(this, nb, NoReqs)); nbs }
  def >*(nbs: NodeBuilderGroup, linkReqs: Seq[Requirement]): NodeBuilderGroup = { nbs.elements.foreach(nb => NodeLinkBuilder(this, nb, linkReqs)); nbs }

  // Node modifiers
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

  private[puzzle] def addTo(link: NodeLinkBuilder) {
    assert(link.dest != this) // cannot go to self
    if (to.map(_.dest).contains(link.dest)) { throw new IllegalPuzzleStructure(s"Node ${this.toString} already points to ${to.toString}") }
    to.append(link)
    link.dest.addFrom(link)
  }
  private[puzzle] def addFrom(link: NodeLinkBuilder) {
    assert(link.src != this) // cannot come from self
    if (from.map(_.src).contains(link.src)) { throw new IllegalPuzzleStructure(s"Node ${this.toString} already points to ${from.toString}") }
    from.append(link)
  }

  private[this] def createNode() = {
    if (isStart) new StartNode(this) else if (isEnd) new EndNode(this) else new Node(this)
  }

  override def toString = s"NodeBuilder(id:$id, contents:${contents.size} reqs:${occupyReqs.size} from:${from.size} to:${to.size} start:$isStart end:$isEnd)"

  // TODO: Keeping this map indefinitely is wasteful. Puzzles should be build with a context that replaces this static NodeBuilder.nodes
  def make: Node = NodeBuilder.nodes.synchronized { NodeBuilder.nodes.getOrElse(this, createNode()) }
}

object NodeBuilder {
  private[puzzle] val nodes = mutable.Map[NodeBuilder, Node]()
  private[puzzle] val nodeLinks = mutable.Map[NodeLinkBuilder, NodeLink]()
  val nextId = new AtomicInteger(0)

  val NoReqs = Seq[Requirement]()

  def apply() = new NodeBuilder(Seq[Requirement]())
  def apply(occupyReqs: Seq[Requirement]) = new NodeBuilder(occupyReqs)
  def apply(occupyReq: Requirement) = new NodeBuilder(Seq[Requirement](occupyReq))

  implicit def seqToNodeBuilderSeq(nbs: Seq[NodeBuilder]): NodeBuilderGroup = NodeBuilderGroup(nbs)
  implicit def listToNodeBuilderSeq(nbs: List[NodeBuilder]): NodeBuilderGroup = NodeBuilderGroup(nbs)
}