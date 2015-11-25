
package problems

import scala.annotation.tailrec
import common.MyMath

object Problem020 extends Problem with App {
  def number = 20
  def description = "Find the sum of the digits in 100 factorial"
  lazy val run = sumOfFact(target)

  lazy val target = 100
  def sumOfFact(n: Int) = MyMath.asListOfDigits(MyMath.factorial(n)).map(_.longValue).sum

  println(run)
}