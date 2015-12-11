package problems

import common.MyMath
import common.myMath.ContinuedFraction
import common.myMath.ContinuedFractionGenerator

object Problem064 extends Problem with App {

  def number = 64
  def description = "How many continued fractions for N <= 10000 have an odd period?"
  lazy val run = oddPeriodCount(Target).longValue()

  lazy val Target = 10000

  def oddPeriodCount(upperBound: Int): Int = (1 to upperBound).par.map(rootContinuedFraction2).count(_.period % 2 == 1)

  // Attempt 1: runs in about 200 seconds. Requires square root represented as high precision BigDecimal. 
  // Most time is spent finding the square root
  def rootContinuedFraction(n: Int): ContinuedFraction = {
    val BasePrecision = 150
    val PrecisionModifierConstant = 30
    val sqRtPrecision = BasePrecision + n / PrecisionModifierConstant

    val root = MyMath.bigSqrt(sqRtPrecision)(n)
    ContinuedFractionGenerator.asContinuedFraction(root)
  }

  // Attempt 2: borrowing from Euler
  def rootContinuedFraction2(n: Int): ContinuedFraction = {
    ContinuedFractionGenerator.asContinuedFraction2(n)
  }

  // Solution from Euler. About 1 second
  // defined at https://projecteuler.net/thread=64;page=7
  def getNums(n: Int, t: Int, d: Int, pastResults: Set[(Int, Int, Int)], aggResult: List[Int]): List[Int] = {
    val d2 = (n - t * t) / d
    val n2 = ((Math.sqrt(n) - t) * 1 / d2).toInt
    val t2 = -t - d2 * n2
    if (pastResults.contains(n2, t2, d2)) aggResult
    else getNums(n, t2, d2, pastResults + ((n2, t2, d2)), aggResult :+ n2)
  }

  def oddPeriod(x: Int) = !Math.sqrt(x).isWhole() && getNums(x, 0, 1, Set(), List()).length % 2 == 1

  val res = (1 to 10000).count(oddPeriod)

  ///////////////// tests /////////////

  println(run)

  //  println(res)

}