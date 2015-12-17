package problems
import common.math.Primes
import common.upgrades.Implicits._

object Problem076 extends Problem with App {
  def number = 76
  def description = "How many different ways can one hundred be written as a sum of at least two positive integers?"

  def run = summations(target)
  
  val target = 100

  def summations(n: Int) = {
    val range = (n - 1 to 1 by -1).toList

    combinations(n, range)
  }

  // First attempt, using standard recursion. Standard recursion seems to be the best solution here,
  // because the list is split and recursion must take place on both parts
  // Solution borrowed from Problem 31. TODO move this to a common library.
  // Runs in 18.3 seconds. Could go faster with memoization
  def combinations(remainingAmount: Int, valsToTry: List[Int]): Long = {
    if (remainingAmount == 0)
      1
    else if (valsToTry.isEmpty || remainingAmount < 0)
      0
    else
      combinations(remainingAmount - valsToTry.head, valsToTry) + combinations(remainingAmount, valsToTry.tail)
  }

  time(summations)(target)
}