
package problems

import common.MyMath.primes

object Problem060 extends Problem with App {
  def number = 60
  def description = "Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime."

  lazy val run = -1L

  def concat(n: Long, m: Long): Long = (n.toString() + m.toString()).toLong

  // TODO define this in a common list function
  def primeContains(n: Long): Boolean = {
    primes.takeWhile(_ <= n).exists(_ == n)
  }
  //  def monotonicContains()
  
  def splork() = primes


  println(run)
}