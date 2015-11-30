
package problems

import scala.annotation.tailrec

object Problem031 extends Problem with App {
  def number = 31
  def description = "How many different ways can £2 (200p) be made using any number of coins, valued at 1p, 2p, 5p, 10p, 20p, 50p, 100p and 200p?"
  lazy val run = countChange(targetAmount, denominations).longValue()

  lazy val targetAmount = 200
  lazy val denominations = List(1, 2, 5, 10, 20, 50, 100, 200)

  // First attempt, using standard recursion. Standard recursion seems to be the best solution here,
  // because the list is split and recursion must take place on both parts
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (coins.isEmpty || money < 0)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  println(run)
}