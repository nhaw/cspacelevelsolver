package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color.Color

import java.io.PrintStream
import scala.collection.immutable


case class PuzzleState(inventory: immutable.Seq[Color], world: Requirement.EntityStates)

object Puzzle {
  def apply(anyNode: Node, name: String) = new Puzzle(anyNode, name)

  def validName(name: String): Boolean = name.matches("[a-zA-Z0-9_]+")
}

@SerialVersionUID(0)
class Puzzle(anyNode: Node, val name: String) extends Serializable {

  assert(Puzzle.validName(name), s"""Invalid puzzle name "$name"""")

  val startNode = Node.findStart(anyNode)
  val startNodeLink = startNode.map(n => NodeLink(null, n, Seq()))
  val endNodes = Node.findEnds(anyNode)
  val valid = startNodeLink.isDefined && endNodes.nonEmpty
  val (entities, pickups, switches, size) = {
    var entitiesSet = immutable.Set[Entity]()
    var pickupSet = immutable.Set[Color]()
    var switchSet = immutable.Set[Switch]()
    var nodeCount = 0
    Node.walk(anyNode,
      n => {
        nodeCount+=1
        n.reqs.foreach(_.subjects.foreach(entitiesSet += _))
        n.contents.foreach(pickupSet += _)
        n.switches.foreach(switchSet += _)
        true
      } )
    (entitiesSet.toList, pickupSet, switchSet, nodeCount)
  }
  val players = entities.collect{case (e: Player) => e.asInstanceOf[Player]}.toSet
  val initialInventory: immutable.Seq[Color] = immutable.Seq[Color]()
  val initialColors: Requirement.EntityStates = entities.map{e => e -> e.initialState}.toMap
  val initialState: PuzzleState = new PuzzleState(initialInventory, initialColors)

  def assertValid() {
    if (startNodeLink.isEmpty) throw new IllegalArgumentException("No start node found")
    if (endNodes.isEmpty) throw new IllegalArgumentException("No end nodes found")
    assert(valid, "Invalid for unlisted reason")
  }

  override def toString = s"Puzzle(nodes:$size ents:${entities.size} players:${players.size}})"

  /**
    * Generate 'dot' graph description
    */
  def writeDot(printStream: PrintStream) {
    printStream.println(s"digraph $name {")
        printStream.println("  START [color=green, shape=diamond]")
        printStream.println("  END [color=green, shape=diamond]")
        startNodeLink.foreach { snl =>
          Node.walk(snl.dest, n => {
            n match {
              case startNode: StartNode => printStream.println(s"  START -> n${n.id};")
              case endNode: EndNode => printStream.println(s"  n${n.id} -> END;")
              case _ =>
            }
            n.to.foreach { nt =>
              printStream.println(s"  n${n.id} [shape=box];")
              printStream.println(s"  n${n.id} -> n${nt.dest.id} [color=black, weight=1];")
            }
            n.reqs.foreach { r =>
              r.subjects.foreach {
                case p: Player =>
                  // Do not print player part of each requirement
                case ent =>
                  val name = s"${ent.name}_${ent.id}"
                  printStream.println(s"  n${n.id} -> $name [dir=none, weight=2];")
                  val bgColor = ent.initialColor.lightenToHex
                  val textColor = if (ent.initialColor == Color.BLACK) "white" else "black"
                  printStream.println(s"""  $name [shape=ellipse,style=filled,fillcolor="$bgColor",textcolor="$textColor"];""")
                  //printStream.println(s"""  {rank=same; $name; n${n.id}}""")
              }
              r match {
                case req: ReqNoInteraction =>
                  if (req.a.isInstanceOf[Player])
                    printStream.println(s"""  ${req.b.name}_${req.b.id} [label="${req.b.name}_${req.b.id} !~ PLAYER"];""")
                  else if (req.b.isInstanceOf[Player])
                    printStream.println(s"""  ${req.a.name}_${req.a.id} [label="${req.a.name}_${req.a.id} !~ PLAYER"];""")
                  else
                    printStream.println(s"""  ${req.a.name}_${req.a.id} -> ${req.b.name}_${req.b.id} [dir=both, style=dotted, label="!~"];""")
                case req: ReqInteraction =>
                  if (req.a.isInstanceOf[Player])
                    printStream.println(s"""  ${req.b.name}_${req.b.id} [label="${req.b.name}_${req.b.id} ~ PLAYER"];""")
                  else if (req.b.isInstanceOf[Player])
                    printStream.println(s"""  ${req.a.name}_${req.a.id} [label="${req.a.name}_${req.a.id} ~ PLAYER"];""")
                  else
                    printStream.println(s"""  ${req.a.name}_${req.a.id} -> ${req.b.name}_${req.b.id} [dir=both, style=dotted, label="~"];""")
                case req =>
                  // Don't know about this interaction interact
                  Console.err.println("Don't know how to print unknown interaction type: " + req)
              }
            }
            /*n.reqs.flatMap(_.subjects) foreach {
              case p: Player =>
              case ent: Entity =>
                printStream.println(s"  n${n.id} -> ${ent.name}_${ent.id};")
                val bgColor = ent.initialColor.lightenToHex
                val textColor = if (ent.initialColor == Color.BLACK) "white" else "black"
                printStream.println(s"""  ${ent.name}_${ent.id} [shape=ellipse,style=filled,fillcolor="$bgColor",textcolor="$textColor"];""")
            }*/
            n.contents.foreach { inv =>
              val name = s"inv_${inv.toString}"
              printStream.println(s"  n${n.id} -> $name [dir=none, style=dotted, weight=2];")
              val bgColor = inv.lightenToHex
              val textColor = if (inv == Color.BLACK) "white" else "black"
              printStream.println(s"""  $name [shape=invhouse,style=filled;fillcolor="$bgColor",textcolor=$textColor];""")
              //printStream.println(s"""  {rank=same; $name; n${n.id}}""")
            }
            n.switches.foreach { sw =>
              val name = s"sw_${sw.toString}"
              printStream.println(s"  n${n.id} -> $name [dir=none, weight=2];")
              printStream.println(s"  $name [shape=triangle];")
              //printStream.println(s"""  {rank=same; $name; n${n.id}}""")
            }
            true
            }
        )
    }
    printStream.println()
    printStream.println("}")
  }
}