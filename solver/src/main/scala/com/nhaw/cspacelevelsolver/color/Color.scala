/**
 * Created by nhaw on 7/11/15.
 */

package com.nhaw.cspacelevelsolver.color

class Color private[color] (val name: String, val r: Float, val g: Float, val b: Float) {

  def interactsWith(c: Color): Boolean = (r > 0 && c.r > 0) || (g > 0 && c.g > 0) || (b > 0 && c.b > 0)

  def ~(c: Color) = interactsWith(c)
  def !~(c: Color) = !interactsWith(c)

  lazy val interactions = Color.getInteractions(this)

  private[this] def toByte(v: Float) = f"${math.min(math.max(v*255, 0), 255).toByte}%02x"

  @inline def toHex = {
    s"#${toByte(r)}${toByte(g)}${toByte(b)}"
  }

  def lightenToHex = {
    s"#${toByte(r+0.3f)}${toByte(g+0.3f)}${toByte(b+0.3f)}"
  }

  override def toString = name
}

object Color {
  case object BLACK  extends Color("black",  0, 0, 0)
  case object RED    extends Color("red",    1, 0, 0)
  case object GREEN  extends Color("green",  0, 1, 0)
  case object BLUE   extends Color("blue",   0, 0, 1)
  case object YELLOW extends Color("yellow", 1, 1, 0)
  case object CYAN   extends Color("cyan",   0, 1, 1)
  case object VIOLET extends Color("violet", 1, 0, 1)
  case object WHITE  extends Color("white",  1, 1, 1)
  case object METAL  extends Color("metal",  0.5f, 0.5f, 0.5f)

  val all = List(BLACK, RED, GREEN, BLUE, YELLOW, CYAN, VIOLET, WHITE, METAL)

  def getInteractions(color:Color) = Color.all.filter(color.interactsWith)
}
