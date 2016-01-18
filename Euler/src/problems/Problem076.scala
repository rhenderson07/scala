package problems

object Problem076 extends Problem with App {
  def number = 76
  def description = "How many different ways can one hundred be written as a sum of at least two positive integers?"

  def run = sumCount2(target)

  val target = 100

  // First attempt, using standard recursion. Standard recursion seems to be the best solution here,
  // because the list is split and recursion must take place on both parts.
  // Solution borrowed from Problem 31. TODO move this to a common library.
  // Runs in 6.0 seconds. Could go faster with memoization.
  def sumCount(n: Int) = {
    def rec(remaining: Int, valsToTry: List[Int]): Long = {
      if (remaining == 0) 1
      else if (valsToTry.isEmpty || remaining < 0) 0
      else rec(remaining - valsToTry.head, valsToTry) + rec(remaining, valsToTry.tail)
    }

    val range = (n - 1 to 1 by -1).toList
    rec(n, range)
  }

  // Second attempt. Caching solution. Runs in 0.03 seconds. Extremely ugly, non-functional code. 
  def sumCount2(n: Int) = {
    val cache = scala.collection.mutable.Map[(Int, Int), Long]().withDefaultValue(0)

    def rec(remaining: Int, valsToTry: List[Int]): Long = {
      def recInner() = {
        if (remaining == 0) 1
        else if (valsToTry.isEmpty || remaining < 0) 0
        else rec(remaining - valsToTry.head, valsToTry) + rec(remaining, valsToTry.tail)
      }

      cache.getOrElseUpdate((remaining, valsToTry.size), recInner)
    }

    val range = (n - 1 to 1 by -1).toList
    val result = rec(n, range)

    println("Cache size: " + cache.size)

    result
  }

  // Caching solution from Euler. Very fast. Runs in 0.035 seconds. Not functional.
  def sumCount3(n: Long) = {
    val cache = scala.collection.mutable.Map[(Long, Long), Long]().withDefaultValue(0)
    
    for {
      i <- 2L to n;
      k <- (i / 2) to 1 by -1
    } {
      cache((i, k)) = 1 + cache((i - k, k)) + cache((i, k + 1))
    }
    val result = cache(n, 1)

    println("Cache size: " + cache.size)

    result
  }

  // Test output
  time(sumCount2)(target)
}