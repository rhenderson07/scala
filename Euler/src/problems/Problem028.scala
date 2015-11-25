
package problems

import common.MyMath
import common.Point

object Problem028 extends Problem with App {
  def number = 28
  def description = "What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed by starting with the number 1 and moving to the right in a clockwise direction?"
  lazy val run = spriralDiagSum(1001)

  def findLocation(value: Int): Point = {
    val lastRoot = MyMath.intSqrt(value - 1)
    val lastSquare = lastRoot * lastRoot

    val nextOrCurrentRoot = MyMath.intSqrtRoundUp(value)
    val nextOrCurrentSquare = nextOrCurrentRoot * nextOrCurrentRoot

    val lastRootEven = lastRoot % 2 == 0
    val halfWayToNextSquare = lastSquare + (nextOrCurrentSquare - lastSquare) / 2
    val underHalfWayToNextRoot = value <= halfWayToNextSquare
    
    // distance from center of spiral
    val ringDist = lastRoot / 2

    if (lastRootEven && underHalfWayToNextRoot)
      //up
      new Point(-ringDist, -ringDist + (value - lastSquare))
    else if (lastRootEven && !underHalfWayToNextRoot)
      //right
      new Point(-ringDist + (value - halfWayToNextSquare) - 1, ringDist)
    else if (!lastRootEven && underHalfWayToNextRoot)
      //down
      new Point(ringDist + 1, -ringDist + (value - lastSquare) - 1)
    else
      //left
      new Point(ringDist + (halfWayToNextSquare - value) + 2, -ringDist - 1)
  }

  def numSpiral(dimensionSize: Int) = {
    val dimSquare = dimensionSize * dimensionSize
    (1 to dimSquare).par.map(x => (findLocation(x), x))
  }

  def spriralDiagSum(n: Int): Long = {
    numSpiral(n).filter(x => isDiagonal(x._1)).map(_._2.longValue()).sum
  }

  def isDiagonal(p: Point): Boolean = {
    Math.abs(p.x) == Math.abs(p.y)
  }

  val t0 = System.nanoTime();
  val sol = spriralDiagSum(5)
  val t1 = System.nanoTime();
  println(s" spiral diagonal sum = $sol.  Elapsed time ${(t1 - t0) * 1e-9} seconds")
}