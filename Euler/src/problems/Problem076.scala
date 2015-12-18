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
    def rec(remaining: Int, valsToTry: List[Int]): Long = {
      if (remaining == 0) 1
      else if (valsToTry.isEmpty || remaining < 0) 0
      else rec(remaining - valsToTry.head, valsToTry) + rec(remaining, valsToTry.tail)
    }

    val range = (n - 1 to 1 by -1).toList
    rec(n, range)
  }

  // Second attempt. Caching solution. Very fast. Extremely ugly code. Runs in 0.03 seconds
  def summations2(n: Int) = {
    val cache = scala.collection.mutable.Map[(Int, Int), Long]().withDefaultValue(0)

    def rec(remaining: Int, valsToTry: List[Int]): Long = {

      def recInner() = {
        //println(s"Inner: $remaining, ${valsToTry.size}")
        if (remaining == 0) 1
        else if (valsToTry.isEmpty || remaining < 0) 0
        else rec(remaining - valsToTry.head, valsToTry) + rec(remaining, valsToTry.tail)
      }

      cache.getOrElseUpdate((remaining, valsToTry.size), recInner)
    }

    val range = (n - 1 to 1 by -1).toList
    val result = rec(n, range)
    
    println(cache.size)
    
    result
  }

  // Caching solution from Euler. Very fast. Run in 0.035 seconds. Not functional.
  def summations3(n: Long) = {
    val cache = scala.collection.mutable.Map[(Long, Long), Long]().withDefaultValue(0)
    for { i <- 2L to n; k <- (i / 2) to 1 by -1 } {
      cache((i, k)) = 1 + cache((i - k, k)) + cache((i, k + 1))
    }
    val result = cache(n, 1)
    
    println(cache.size)
    
    result
  }

  time(summations2)(target)
}