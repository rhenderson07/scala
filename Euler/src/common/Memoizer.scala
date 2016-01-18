package common

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

  //unfinished attempt to allow memoization with multiple parameters 
  //  def apply[A, T, R](fRec: (A, T, (A, T) => R) => R): (A, T => R) = {
  //    def f(a: A, n: T): R = fRec(a, n, (a, n) => f(a, n))
  //    Memoizer(f)
  //  }
}