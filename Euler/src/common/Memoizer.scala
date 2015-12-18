package memoization

/**
 * This is more complex memoizer that is able to work with recursive functions.
 */
case class Memoizer[-T, +R](f: T => R) extends (T => R) {
  import scala.collection.mutable
  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T): R = vals.getOrElseUpdate(x, f(x))
}

object RecursiveMemomizedFunction {
  def apply[T, R](fRec: (T, T => R) => R): (T => R) = {
    def f(n: T): R = fRec(n, n => f(n))
    Memoizer(f)
  }
}