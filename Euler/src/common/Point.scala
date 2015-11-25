package common

class Point(val x: Int, val y: Int) extends Equals {
  override def toString() = {
    "[%d,%d]".format(x, y)
  }

  def canEqual(other: Any) = {
    other.isInstanceOf[common.Point]
  }

  override def equals(other: Any) = {
    other match {
      case that: common.Point => that.canEqual(Point.this) && x == that.x && y == that.y
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + x.hashCode) + y.hashCode
  }
}