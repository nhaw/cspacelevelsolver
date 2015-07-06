/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color.Color

import scala.collection._

case class EntityState(color: Color, enabled: Boolean) {
  def ~ (ent: EntityState) = color ~ ent.color && enabled == ent.enabled
  def !~ (ent: EntityState) = ! this.~(ent)

  def withColor(newColor: Color) = EntityState(newColor, enabled)
  def withEnabled(newEnabled: Boolean) = EntityState(color, newEnabled)
}

object Requirement {
  type EntityStates = Map[Entity,EntityState]
  type EntitySet = Set[Entity]
}

trait Requirement {
  def satisfied(implicit entityStates: Requirement.EntityStates): Boolean
  def subjects: Requirement.EntitySet
}

case class ReqInteraction(a: Entity, b: Entity) extends Requirement {
  require(!(a eq b), "a and b cannot be the same entity")
  override def toString = {s"$a ~ $b"}
  override def satisfied(implicit entityStates: Requirement.EntityStates) = entityStates(a) ~ entityStates(b)
  override def subjects = immutable.Set[Entity](a,b)
}

case class ReqNoInteraction(a: Entity, b: Entity) extends Requirement {
  override def toString = {s"$a !~ $b"}
  override def satisfied(implicit entityStates: Requirement.EntityStates) = entityStates(a) !~ entityStates(b)
  override def subjects = immutable.Set[Entity](a,b)
}
