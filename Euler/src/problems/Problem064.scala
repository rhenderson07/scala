package problems

import common.MyMath
import scala.math.BigDecimal.RoundingMode

object Problem064 extends Problem with App {
  def number = 64
  def description = "How many continued fractions for N <= 10000 have an odd period?"
  lazy val run = oddPeriodCount(Target).longValue()

  lazy val Target = 10000

  def oddPeriodCount(upperBound: Int) = (1 to upperBound).map(asContinuedFraction(_)).count(_.period % 2 == 1)

  def asContinuedFraction(n: BigDecimal): ContinuedFraction = {
    val wholePart = n.setScale(0, RoundingMode.DOWN).toInt
    val remainder = n - wholePart

    new ContinuedFraction(wholePart, List())
  }

  def continuedFractionGenerator(n: BigDecimal): Stream[BigDecimal] = {
    Stream.iterate(n)(nextContinuedFractionPart)
  }

  private def nextContinuedFractionPart(n: BigDecimal): BigDecimal = {
    val wholePart = n.setScale(0, RoundingMode.DOWN).toInt
    val remainder = n - wholePart

    remainder match {
      case d: BigDecimal if d != 0 => 1 / d
      case _ => 0
    }
  }

  class ContinuedFraction(val wholePart: Int, val repeatedBlock: List[Int]) {
    val period = repeatedBlock.length

    override def toString(): String = {
      s"[$wholePart; $repeatedBlock]"
    }
  }

  println(run)
  
  
val testVal =MyMath.sqrt(23)
val generator = continuedFractionGenerator(testVal)
generator.take(20).map(_.setScale(0, RoundingMode.DOWN).toInt).foreach(println)
}