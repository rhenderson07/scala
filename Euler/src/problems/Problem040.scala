
package problems

import common.MyMath

object Problem040 extends Problem with App {
  def number = 40
  def description = """Champernowne's constant is produced by concatenating the positive integers: 0.123456789101112....
If dn represents the nth digit of the fractional part, find the value of the following expression:
"d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000""""

  lazy val run = -1L

  lazy val champernowne = Stream.from(1).map(MyMath.asListOfDigits(_)).flatten

  def champernowneProduct(n: Int) = {
    (0 to n).map(p => champernowne(MyMath.power(10, p).toInt - 1)).product
  }

  println(champernowne(10000000))
}