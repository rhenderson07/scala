
package problems

import scala.annotation.tailrec
import common.Lists
import common.MyMath

object Problem060 extends Problem with App {
  def number = 60
  def description = "Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime."

  lazy val run = findPrimeFamily_Fail1(3).sum

  lazy val primes = MyMath.primes

  def concat(n: Long, m: Long): Long = (n.toString() + m.toString()).toLong

  // TODO define this in a common list function
  def primesContains(n: Long): Boolean = {
    primes.takeWhile(_ <= n).contains(n)
  }

  //  val isPrimePair = (n: Long, m: Long) => primesContains(concat(n, m)) && primesContains(concat(m, n))
  def isPrimePair(n: Long, m: Long) = primesContains(concat(n, m)) && primesContains(concat(m, n))
  def isPrimeFamily = Lists.holdsForFamily(isPrimePair)_

  // first attempt. slow, using tail recursion. ~80 seconds to find family of 4.
  def findPrimeFamily_Fail1(targetSetSize: Int) = {
    @tailrec
    def rec(remainingPrimes: Stream[Long], currentSet: List[List[Long]] = List(List())): List[List[Long]] = {
      if (remainingPrimes.isEmpty || currentSet.exists(_.length == targetSetSize)) {
        currentSet
      } else {
        val candPrime = remainingPrimes.head
        // append cand to front of all lists it pairs with
        val newSets = currentSet.par.filter(_.forall(isPrimePair(_, candPrime))).map(candPrime :: _).toList

        rec(remainingPrimes.tail, newSets ::: currentSet)
      }
    }

    rec(primes).maxBy(_.length).reverse
  }

  // second attempt. faster, using streams. ~40 seconds to find a family of 4
  def findPrimeFamily_Fail2(size: Int, cap: Long = 1000) = primes.takeWhile(_ < cap).combinations(size).find(isPrimeFamily(_))
  // println(findPrimeFamily(3, 1000))

  // third attempt. try to cheat by only using the known family. Does not return.
  val knownPrimeFamily = List(3, 7, 109, 673)
  def findPrimeFamily_Fail3() = primes.find(x => knownPrimeFamily.forall(y => isPrimePair(x, y)))

  // fourth attempt. Avoid checking pair more than once
  val dimensionLength = 100
  val bigArray = Array.tabulate[Boolean](dimensionLength, dimensionLength)((x, y) => if (x < y) isPrimePair(primes(x), primes(y)) else false)
  println(bigArray(1)(1))

}