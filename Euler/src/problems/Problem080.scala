
package problems

import common.math.MyMath

object Problem080 extends Problem with App {
  def number = 80
  def description = "For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots."

  lazy val run = rootSumsTo(targetScope)(targetRange).toLong

  val targetScope = 100
  val targetRange = 100

  def rootSumsTo(scope: Int)(range: Int) = {
    val accuracyBuffer = 20

    (1 to range)
      .map(x => MyMath.bigSqrt(scope + accuracyBuffer)(x))
      .filterNot(_.isValidInt)
      .map(d => MyMath.asListOfDigits(d).take(scope)).flatten
      .sum
  }

  println(run)
}