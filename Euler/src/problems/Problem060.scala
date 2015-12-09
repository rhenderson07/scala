
package problems

import scala.annotation.tailrec
import common.MyMath
import my_collections.MyTraversable
import my_collections.MyTraversable

object Problem060 extends Problem with App {
  def number = 60
  def description = "Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime."
  implicit def listToMyTraversable[T](l: Traversable[T]) = new MyTraversable(l)

  lazy val run = splork3(5).sum
  
  lazy val primes = MyMath.primes

  def concat(n: Long, m: Long): Long = (n.toString() + m.toString()).toLong

  // TODO define this in a common list function
  def primesContains(n: Long): Boolean = {
    primes.takeWhile(_ <= n).contains(n)
  }

//  val isPrimePair = (n: Long, m: Long) => primesContains(concat(n, m)) && primesContains(concat(m, n))
  def isPrimePair (n: Long, m: Long) = primesContains(concat(n, m)) && primesContains(concat(m, n))

  def splork3(targetSetSize: Int) = {
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
  
//  def isPrimeSet = MyTraversable.
  
//  primes.findSet(_.holdsForFamily(isPrimePair))(3)

  println(run)
}