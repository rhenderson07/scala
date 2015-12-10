package problems

import common.MyMath
import scala.math.BigDecimal.RoundingMode
import common.myMath.ContinuedFraction
import common.myMath.ContinuedFractionGenerator
import common.myMath.ContinuedFraction

object Problem064 extends Problem with App {

  def number = 64
  def description = "How many continued fractions for N <= 10000 have an odd period?"
  lazy val run = oddPeriodCount(Target).longValue()

  lazy val Target = 13

  def oddPeriodCount(upperBound: Int) = (1 to upperBound).map(rootContinuedFraction).count(_.period % 2 == 1)

  def rootContinuedFraction(n: Int): ContinuedFraction = {
    val BigDecPrecision = 150
    val root = MyMath.bigSqrt(BigDecPrecision)(n)
    ContinuedFractionGenerator.asContinuedFraction(root)
  }

  //  println(run)

  //test for specific value
  println(rootContinuedFraction(552))

  //test for range
  (1 to 1000).foreach(x => println(x, rootContinuedFraction(x)))
}