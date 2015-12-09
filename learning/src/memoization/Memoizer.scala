package memoization

/**
 * Scala does not implement memoization directly, but has a collection method named "getOrElseUpdate()"
 * that handles most of the work of implementing it.
 */
object Memoizer extends App {
  // general technique for converting a function to a memoized function
  def memoize[A, B](f: A => B) = new (A => B) {
    val cache = scala.collection.mutable.Map[A, B]()
    def apply(x: A): B = {
      cache.getOrElseUpdate(x, f(x))
    }
  }

  // This only works if the memoized function is a val, not a def.
  // TODO Check StackOverflow to find alternative.
  val sumOfFactors = memoize(sumFactors)

  def sumFactors(n: Int) = {
    factorsOf(n).sum
  }

  def factorsOf(n: Int) = {
    Stream.from(1).takeWhile(_ < n).filter(n % _ == 0)
  }

  val values = List(1, 2, 1000000, 1000000, 1000000, 1000001, 1000001, 1000001, 1000001, 1000000, 1000000, 1000000, 1000000, 1000000, 1000000)
  values.foreach(x => println(x + ": " + sumOfFactors(x)))
}