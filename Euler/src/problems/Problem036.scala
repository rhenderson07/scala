
package problems

import scala.annotation.tailrec
import common.Strings

object Problem036 extends Problem with App {
  def number = 36
  def description = "Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2."
  lazy val run = doublePalindromes.takeWhile(_ < targetUpperBound).map(_.longValue()).sum

  lazy val targetUpperBound = 1000000

  def isDoublePalindrome(n: Long): Boolean = Strings.isPalindrome(n.toString) && Strings.isPalindrome(n.toBinaryString)
  lazy val doublePalindromes = Stream.from(0).filter(isDoublePalindrome(_))

  println(run)
  
  //doublePalindromes.take(20).map(x=> (x, x.toBinaryString)).foreach(println)
}