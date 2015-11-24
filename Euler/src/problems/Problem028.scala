
package problems

import common.MyMath

object Problem028 extends Problem with App {
  def number = 28
  def description = "What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed by starting with the number 1 and moving to the right in a clockwise direction?"
  def run = -1

  val squareEdgeLen = 1001

  class Point(val x: Int, val y: Int) {
    override def toString() = {
      "[%d,%d]".format(x, y)
    }
  }

  def findLocation(value: Int): Point = {
    val lastRoot = MyMath.intSqrt(value - 1)
    val lastSquare = lastRoot * lastRoot

    val nextOrCurrentRoot = MyMath.intSqrtRoundUp(value)
    val nextOrCurrentSquare = nextOrCurrentRoot * nextOrCurrentRoot

    val lastRootEven = lastRoot % 2 == 0
    val halfWayToNextSquare = lastSquare + (nextOrCurrentSquare - lastSquare) / 2
    val underHalfWayToNextRoot = value <= halfWayToNextSquare

    if (lastRootEven && underHalfWayToNextRoot)
      //up
      new Point(-lastRoot / 2, -lastRoot / 2 + (halfWayToNextSquare - value))
    else if (lastRootEven && !underHalfWayToNextRoot)
      //right
      new Point(-lastRoot / 2 + (value - halfWayToNextSquare) - 1, lastRoot / 2)
    else if (!lastRootEven && underHalfWayToNextRoot)
      //down
      new Point(lastRoot / 2 + 1, (halfWayToNextSquare - value) - lastRoot / 2)
    else
      //left
      new Point(lastRoot / 2 + 2 + (halfWayToNextSquare - value), -lastRoot / 2 - 1)
  }

  def numSpiral(dimensionSize: Int) = {
    val dimSquare = dimensionSize * dimensionSize
    (1 to dimSquare).map(x => (findLocation(x), x))
  }

  def spriralDiagSum(n: Int): Long = numSpiral(n).filter(x => isDiagonal(x._1)).map(_._2.longValue()).sum

  def isDiagonal(p: Point): Boolean = {
    Math.abs(p.x) == Math.abs(p.y)
  }

  println(numSpiral(5))
  println(spriralDiagSum(5))
}