/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import java.util.concurrent.atomic.AtomicInteger

import com.nhaw.cspacelevelsolver.color.Color

/**
 * An entity which cannot have its com.nhaw.cspacelevelsolver.color changed
 */
trait FixedColor

abstract class Entity(val initialColor: Color, val initialEnabled: Boolean) {
  val id = Entity.nextId
  def name:String

  override def toString = s"Entity($id,$name)"
  override def equals(o: Any) = o match {
    case that: Player => that.id == id
    case _ => false
  }
  override def hashCode = id.hashCode

  val initialState = EntityState(initialColor, initialEnabled)
}

/**
 * A player representation
 */
case class Player(initColor: Color) extends Entity(initColor, true) {
  override val name = "player"
}

/**
 * A platform onto which a player can move
 */
class Platform(initColor: Color, _initEnabled: Boolean) extends Entity(initColor, _initEnabled) {
  override val name = "platform"

  def asDisabled() = new Platform(initColor, false)
}

/**
 * An object that impedes the players progress but onto which the player is not intended walk
 */
class Blocker(initColor: Color, _initEnabled: Boolean) extends Entity(initColor, _initEnabled) {
  override val name = "blocker"

  def asInaccessible() = new Platform(initColor, false)
}

/**
 * A laser that blocks a players progress. Note that this type of laser may have a physical emitter which can accept
 * com.nhaw.cspacelevelsolver.color changes but the plyer cannot each it to walk on it.
 */
class LaserBlocker(initColor: Color, _initEnabled: Boolean) extends Blocker(initColor, _initEnabled) {
  override val name = "laser"
}

/**
 * Moves the player automatically between platforms in a way that the player could not do him/herself (e.g a very
 * strong wind flow).
 */
class AutoMoverFixedColor(initColor: Color, _initEnabled: Boolean) extends Entity(initColor, _initEnabled) with FixedColor {
  override val name = "mover_fc"
}

object Entity {
  private[this] val _nextId = new AtomicInteger(0)
  def nextId = _nextId.getAndIncrement()

  def PLAYER   (color: Color) = new Player      (color)
  def PLATFORM (color: Color) = new Platform    (color, _initEnabled=true)
  def BLOCKER  (color: Color) = new Blocker     (color, _initEnabled=true)
  def LASER    (color: Color) = new LaserBlocker(color, _initEnabled=true)
}
