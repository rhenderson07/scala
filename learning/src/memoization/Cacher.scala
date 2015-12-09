package memoization

// This is an example of explicit caching. However, in Scala memoization is preferred to caching.
object Cacher extends App {
  private val sumCache = scala.collection.mutable.Map[Int, Int]()

  // This function uses a cache to improve performance, but the function now has side effects
  def sumOfFactors(number: Int) = {
    sumCache.getOrElseUpdate(number, factorsOf(number).sum)
  }

  def factorsOf(n: Int) = {
    Stream.from(1).takeWhile(_ < n).filter(n % _ == 0)
  }

  val values = List(1, 2, 1000000, 1000000, 1000000, 1000001, 1000001, 1000001, 1000001, 1000000, 1000000, 1000000, 1000000, 1000000, 1000000)
  values.foreach(x => println(x + ": " + sumOfFactors(x)))
}