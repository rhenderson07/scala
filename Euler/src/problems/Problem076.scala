package problems
import common.math.Primes
import common.upgrades.Implicits._

object Problem076 extends Problem with App {
  def number = 76
  def description = "How many different ways can one hundred be written as a sum of at least two positive integers?"

  def run = summations2(target)

  val target = 100

  // First attempt, using standard recursion. Standard recursion seems to be the best solution here,
  // because the list is split and recursion must take place on both parts
  // Solution borrowed from Problem 31. TODO move this to a common library.
  // Runs in 18.3 seconds. Could go faster with memoization
  def summations(n: Int) = {
    def combinations(remainingAmount: Int, valsToTry: List[Int]): Long = {
      if (remainingAmount == 0)
        1
      else if (valsToTry.isEmpty || remainingAmount < 0)
        0
      else
        combinations(remainingAmount - valsToTry.head, valsToTry) + combinations(remainingAmount, valsToTry.tail)
    }

    val range = (n - 1 to 1 by -1).toList
    combinations(n, range)
  }

  // Second attempt. Caching solution. very fast. extremely ugly. Runs in 0.03 seconds
  def summations2(n: Int) = {
    val cache = scala.collection.mutable.Map[(Int, Int), Long]().withDefaultValue(0)

    def rec(remainingAmount: Int, valsToTry: List[Int]): Long = {
      lazy val recInner = {
        if (remainingAmount == 0)
          1
        else if (valsToTry.isEmpty || remainingAmount < 0)
          0
        else
          rec(remainingAmount - valsToTry.head, valsToTry) + rec(remainingAmount, valsToTry.tail)
      }

      cache.getOrElseUpdate((remainingAmount, valsToTry.size), recInner)
    }

    val range = (n - 1 to 1 by -1).toList
    rec(n, range)
  }

  // Caching solution from Euler
  def sol(n: Long) = {
    val cache = scala.collection.mutable.Map[(Long, Long), Long]().withDefaultValue(0)

    for {
      i <- 2L to n;
      k <- (i / 2) to 1 by -1
    } {
      cache((i, k)) = 1 + cache((i - k, k)) + cache((i, k + 1))
    }
    cache(n, 1)
  }

  time(summations2)(100)
}