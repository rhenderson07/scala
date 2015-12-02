
package problems

import common.MyMath

object Problem052 extends Problem with App {
  def number = 52
  def description = "Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits."

  lazy val run = Stream.from(1).map(_.longValue).find(x => (1 to 6).forall(i => sameDigits(x, i * x))).get

  def sameDigits(n: BigInt, m: BigInt): Boolean = {
    val sort1 = MyMath.asListOfDigits(n).sorted
    val sort2 = MyMath.asListOfDigits(m).sorted
    sort1.equals(sort2)
  }

  println(run)
}