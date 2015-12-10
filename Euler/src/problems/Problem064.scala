package problems

import common.MyMath
import common.myMath.ContinuedFraction
import common.myMath.ContinuedFractionGenerator

object Problem064 extends Problem with App {

  def number = 64
  def description = "How many continued fractions for N <= 10000 have an odd period?"
  lazy val run = oddPeriodCount(Target).longValue()

  lazy val Target = 10000

  def oddPeriodCount(upperBound: Int): Int = (1 to upperBound).map(rootContinuedFraction).count(_.period % 2 == 1)

  def rootContinuedFraction(n: Int): ContinuedFraction = {
    val BigDecPrecision = 200
    val root = MyMath.bigSqrt(BigDecPrecision)(n)
    ContinuedFractionGenerator.asContinuedFraction(root)
  }

  //  println(run)

  //test for specific value
  //println(rootContinuedFraction(4846))

  //test for range
  (1 to 10000).foreach(x => println(x, rootContinuedFraction(x)))
}