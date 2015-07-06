/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.puzzle

import com.nhaw.cspacelevelsolver.color._

abstract class Item {
  def name:String
}

case class Gel(color: Color) extends Item {
  override val name = "gel"
}
