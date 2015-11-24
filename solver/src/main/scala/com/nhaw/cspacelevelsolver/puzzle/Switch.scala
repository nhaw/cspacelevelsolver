package com.nhaw.cspacelevelsolver.puzzle

import java.util.concurrent.atomic.AtomicInteger


object SwitchAttributes {
  def apply(numUses: Int) = new SwitchAttributes(numUses)
}

class SwitchAttributes(val numUses: Int) {
  assert(numUses > 0, "numUses <= 0 ($numUses) does not make sense. Must be able to use at least once")
}


object Switch {
  val nextId_ = new AtomicInteger(0)
  def nextId = nextId_.getAndIncrement()
}

/**
 * A switch object that controls other entities by 'enabling' or 'disabling' them (i.e. moving them into or out of a p
 * @param controlTargets Entities which the switch controls
 * @param attributes Attributes describing the usages of the switch
 * @param name Name of this switch
 */
case class Switch(controlTargets: Seq[Entity], attributes: SwitchAttributes, name: String = "<unnamed>") {
  val id = Switch.nextId

  override def toString = s"Entity($id,$controlTargets,$name)"
  override def equals(o: Any) = o match {
    case that: Switch => that.id == id
    case _ => false
  }
  override def hashCode = id.hashCode
}
