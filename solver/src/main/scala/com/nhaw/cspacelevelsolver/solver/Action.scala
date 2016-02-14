package com.nhaw.cspacelevelsolver.solver

import com.nhaw.cspacelevelsolver.color.Color
import com.nhaw.cspacelevelsolver.puzzle.{Entity, PuzzleState}
import collection._

/**
 * Action for progressing through a com.nhaw.cspacelevelsolver.color.puzzle. e.g. use an com.nhaw.cspacelevelsolver.color on an entity
 */
trait Action {
  def resultState(st: PuzzleState): PuzzleState
}

case class RootAction() extends Action {
  def resultState(st: PuzzleState) = ???

  override def hashCode: Int = RootAction.hashCode ^ 1

  override def equals(other: Any): Boolean = other match {
    case other: RootAction => true
    case _ => false
  }
}

case class EndAction() extends Action {
  def resultState(st: PuzzleState) = ???

  override def hashCode: Int = RootAction.hashCode ^ 1

  override def equals(other: Any): Boolean = other match {
    case other: EndAction => true
    case _ => false
  }
}

case class NoAction() extends Action {
  def resultState(st: PuzzleState) = st

  override def hashCode: Int = RootAction.hashCode ^ 1

  override def equals(other: Any): Boolean = other match {
    case other: NoAction => true
    case _ => false
  }
}

case class PickupAction(item: Color) extends Action {
  def resultState(st: PuzzleState) = PuzzleState(st.inventory ++ Seq(item), st.world)

  override def hashCode: Int = RootAction.hashCode ^ 1

  override def equals(other: Any): Boolean = other match {
    case other: PickupAction => item == other.item
    case _ => false
  }
}

// TODO: None of these actions explicitly list pickups
// TODO: None of these actions refer back to the node they occur on (which would show pickups/switches and be generally useful for display - especially with backtracking solutions)

case class ColorChangeAction(color: Color, ent: Entity) extends Action {
  def resultState(st: PuzzleState) = {
    val newInv = {
      var found = false
      st.inventory.filter { c => {
        if (found)
          true
        else if (c != color)
          true
        else {
          found = false
          false
        }
      }
    }
  }
  new PuzzleState(newInv, st.world + (ent -> st.world(ent).withColor(color) ))
  }

  override def hashCode: Int =  color.toString.hashCode ^ ent.id.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: ColorChangeAction => color == other.color && ent.id == other.ent.id
    case _ => false
  }
}

object CompoundColorChangeAction {
  def apply() = new CompoundColorChangeAction(Nil, immutable.HashSet[Entity]())
}

case class CompoundColorChangeAction(actions: List[ColorChangeAction], private val entitiesAffected: immutable.HashSet[Entity]) extends Action {

  def resultState(st: PuzzleState) = {
    actions.foldLeft(st){ case (accState,action) => action.resultState(st) }
  }

  def + (newAction: ColorChangeAction) = {
    assert(!entitiesAffected.contains(newAction.ent))
    CompoundColorChangeAction(newAction :: actions, entitiesAffected + newAction.ent)
  }

  def contains(ent: Entity) = entitiesAffected.contains(ent)

  override def hashCode: Int = if (actions.isEmpty) CompoundColorChangeAction.hashCode else actions.foldLeft[Int](0){ (agg:Int, act:ColorChangeAction) => agg ^ act.hashCode }

  override def equals(other: Any): Boolean = {
    other match {
      case other: ColorChangeAction => actions.size == 1 && actions.head.equals(other)
      case other: CompoundColorChangeAction =>
        actions.size == other.actions.size &&
        actions.sortBy(_.ent.id).zip(other.actions.sortBy(_.ent.id)).foldLeft(true) { case (agg: Boolean, (a: ColorChangeAction, b: ColorChangeAction)) => agg && (a equals b) }
      case _ => false
    }
  }
}
