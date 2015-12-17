package problems

import common.upgrades.Implicits._

object Problem036 extends Problem with App {
  def number = 36
  def description = "Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2."
  lazy val run = doublePalindromes.takeWhile(_ < targetUpperBound).map(_.longValue()).sum

  lazy val targetUpperBound = 1000000

  def isDoublePalindrome(n: Long): Boolean = n.toString.isPalindrome && n.toBinaryString.isPalindrome
  lazy val doublePalindromes = Stream.from(0).filter(isDoublePalindrome(_))

  println(run)

  //doublePalindromes.take(20).map(x=> (x, x.toBinaryString)).foreach(println)
}