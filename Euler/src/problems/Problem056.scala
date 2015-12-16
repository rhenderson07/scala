
package problems

import common.math.MyMath

object Problem056 extends Problem with App {
  def number = 56
  def description = "Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?"

  lazy val run = (for (a <- BigInt(1) to maxPower; b <- 1 to maxPower; val x = MyMath.sumOfDigits(a.pow(b))) yield x).max
  val maxPower = 100

  println(run)
}