
package problems

import scala.BigInt
import common.math.MyMath

object Problem016 extends Problem with App {
  def number = 16
  def description = "What is the sum of the digits of the number 2^1000?"
  lazy val run = MyMath.sumOfDigits(largeVal)

  lazy val largeVal = BigInt(2).pow(1000)

  println(run)
}