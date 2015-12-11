
package problems

import scala.annotation.tailrec
import common.Lists
import common.MyMath
import scala.collection.mutable.HashSet
import scala.util.control.Breaks._

object Problem060 extends Problem with App {
  def number = 60
  def description = "Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime."

  lazy val run = findPrimeFamily_Fail1(3).sum

  lazy val primes = MyMath.primes

  def concat(n: Long, m: Long): Long = (n.toString() + m.toString()).toLong

  //ensure that value is prime. Fairly slow
  def primesContains(n: Long): Boolean = primes.takeWhile(_ <= n).contains(n)
  def isPrimePair(m: Long, n: Long) = primesContains(concat(m, n)) && primesContains(concat(n, m))
  def isPrimeFamily = Lists.holdsForFamily(isPrimePair)_

  // value is probably prime. much faster
  def isProbablyPrimePair(certainty: Int)(m: Long, n: Long) = {
    BigInt(concat(m, n)).isProbablePrime(certainty) && BigInt(concat(n, m)).isProbablePrime(certainty)
  }
  def isProbablyPrimeFamily = Lists.holdsForFamily(isProbablyPrimePair(5))_

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

  // fourth attempt. Avoid checking pair more than once by using a large array to hold the relationship
  val PrimeConfidence = 1
  val MultipleCap = 5
  val ValueCap = 2000
  val dimensionLength = primes.indexWhere(_ > ValueCap)
  lazy val primePairArr = Array.tabulate[Boolean](dimensionLength, dimensionLength) {
    (x, y) =>
      if (primes(x) * MultipleCap < Int.MaxValue && x < y)
        isProbablyPrimePair(PrimeConfidence)(primes(x), primes(y))
      else
        false
  }

  def isPrimeFamilyArray = Lists.holdsForFamily((a: Int, b: Int) => primePairArr(a)(b) == true)_

  lazy val results = {
    for {
      x <- 0 to primePairArr.size - 1;
      y <- x to primePairArr.size - 1;
      z <- y to primePairArr.size - 1;
      a <- z to primePairArr.size - 1;
      //b <- a to primePairArr.size - 1;
      if (isPrimeFamilyArray(List(x, y, z, a /*, b*/ )))
    } yield {
      (primes(x), primes(y), primes(z), primes(a) /*,primes(b)*/ )
    }
  }
  // results.foreach(println)

  // println(bigArray(1)(1))
  // println(primes.takeWhile(_ < 674).size)

  /**
   * Lazy Stream of Lazy Streams example
   */
  {

    val ValueCap = 30000
    lazy val primePairs: Stream[Stream[Long]] = {
      Stream.from(0).map { i =>
        primes
          .takeWhile(n => n < ValueCap)
          .filter(x => x > i) //primes(i))// && isProbablyPrimePair(PrimeConfidence)(primes(i), x))
      }
    }

    println(primePairs(3000).take(8000))
  }

  /**
   * Solution from http://www.mathblog.dk/project-euler-60-primes-concatenate/
   * modified from Java to Scala.
   */
  //  {
  //    def MakePairs( a:Int)  = {
  //    for {
  //      b <- a + 1 to primeList.length;
  //        if (isProbablyPrimePair(PrimeConfidence)(primes(a), primes(b)))
  //    } yield {
  //      primes(b)
  //    }
  //}
  //    
  //var result = Int.MaxValue;
  //val primeList = primes.take(30000);
  //var pairs :HashSet[Int] = HashSet(primeList.size);
  // 
  //for (a <- 1 to primeList.size) { 
  //     if (primes(a) * MultipleCap >= result)
  //       break;
  //     else if (pairs(a) == null) 
  //       pairs(a) = MakePairs(a);
  //    for (b <- a + 1 to primeList.size; b++) { 
  //         if (primes[a] + primes[b] * 4 >= result) break;
  //        if (!pairs[a].Contains(primes[b])) continue;
  //        if (pairs[b] == null) pairs[b] = MakePairs(b);
  // 
  //        for (int c = b + 1; c < primes.Length; c++) { 
  //             if (primes[a] + primes[b] + primes1 * 3 >= result) break;
  //            if (!pairs[a].Contains(primes1) ||
  //            !pairs[b].Contains(primes1)) continue;
  //            if (pairs1 == null) pairs1 = MakePairs(c);
  // 
  //            for (int d = c + 1; d < primes.Length; d++) { 
  //                 if (primes[a] + primes[b] + primes1 + primes[d] * 2 >= result) break;
  //                if (!pairs[a].Contains(primes[d]) ||
  //                !pairs[b].Contains(primes[d]) ||
  //                !pairs1.Contains(primes[d])) continue;
  //                if (pairs[d] == null) pairs[d] = MakePairs(d);
  // 
  //                for (int e = d + 1; e < primes.Length; e++) { 
  //                     if (primes[a] + primes[b] + primes1 + primes[d] + primes[e] >= result) break;
  //                    if (!pairs[a].Contains(primes[e]) ||
  //                    !pairs[b].Contains(primes[e]) ||
  //                    !pairs1.Contains(primes[e]) ||
  //                    !pairs[d].Contains(primes[e])) continue;
  // 
  //                    if (result > primes[a] + primes[b] + primes1 + primes[d] + primes[e])
  //                        result = primes[a] + primes[b] + primes1 + primes[d] + primes[e];
  // 
  //                    Console.WriteLine("{0} + {1} + {2} + {3} + {4} = {5}", primes[a], primes[b], primes1, primes[d], primes[e], result);
  //                    break;
  //                }
  //            }
  //        }
  //    }
  //}
  //  }

}