package memoization

/**
 * This is more complex memoizer that is able to work with recursive functions.
 */
case class Memoizer2[-T, +R](f: T => R) extends (T => R) {
  import scala.collection.mutable
  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T): R = vals.getOrElseUpdate(x, f(x))
}

object RecursiveMemomizedFunction {
  def apply[T, R](fRec: (T, T => R) => R): (T => R) = {
    def f(n: T): R = fRec(n, n => f(n))
    Memoizer2(f)
  }
}

object MemomizeTest extends App {
  def facRec(n: BigInt, f: BigInt => BigInt): BigInt = {
    println("facRec " + n)
    if (n == 0) 1 else n * f(n - 1)
  }
  
  var fac = RecursiveMemomizedFunction(facRec)

  println(fac(5))
  println(fac(5))
}