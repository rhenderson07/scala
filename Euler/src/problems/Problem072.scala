
package problems

import common.MyMath
import scala.collection.LinearSeq
import scala.collection.mutable.HashTable
import scala.collection.parallel.mutable.ParHashMap
import scala.annotation.tailrec

object Problem072 extends Problem with App {
  def number = 72
  def description = "How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?"

  lazy val run = properFractionCountUnder4(target).longValue()
  //  lazy val run = fractionsLessThan(target).size.longValue()

  lazy val target = 1000000

  // First attempt. very slow. 1.6 seconds for target 3000
  {
    def fractionsLessThan(n: Int) = {
      val fractions = for {
        denom <- 1 to n
        num <- 1 until denom
        if (BigInt(num).gcd(denom).equals(1))
      } yield {
        (num, denom)
      }

      fractions //.toList
    }
  }

  // second attempt. 0.9 seconds for target 3000
  {
    def properFractionCountUnder(n: Int) = {
      (1 to n).map(properFactionCount).sum
    }
    def properFactionCount(n: Int) = {
      (1 to n).count(BigInt(n).gcd(_).equals(1))
    }
  }

  // third attempt. Cache factors. slow. 1.8 seconds for target 3000
  {
    lazy val factorCache = scala.collection.mutable.Map[Int, LinearSeq[Long]]()

    def factors(number: Int) = {
      factorCache.getOrElseUpdate(number, MyMath.primeDivisors(number))
    }

    def properFractionCountUnder3(n: Int) = {
      (1 to n).map(properFactionCount3).sum
    }

    def properFactionCount3(n: Int) = {
      (1 to n).count(!shareFactor(_, n))
    }

    def shareFactor(a: Int, b: Int) = {
      factors(a).intersect(factors(b)).nonEmpty
    }
  }

  // attempt to produce primes for all values between 1 and target
  def primeFactorMapTo(n: Int) = {
    for {
      x <- 1 to n
    } yield {
      MyMath.primeDivisors(x)
    }
  }

  /**
   * Adapted solution from MathBlog:
   *
   * int limit = 1000000;
   * int[] phi = Enumerable.Range(0, limit+1).ToArray();
   * long result = 0;
   * for(int i = 2; i <= limit; i++){
   * if (phi[i] == i) {
   * for (int j = i; j <= limit; j += i) {
   * phi[j] = phi[j] / i * (i - 1);
   * }
   * }
   * result += phi[i];
   */
  def properFractionCountUnder4(limit: Int): Long = {
    val phi = scala.collection.mutable.Map() ++ (0 to limit).zipWithIndex.toMap
    var result: Long = 0

    for (i <- 2 to limit) {
      if (i == phi(i)) {
        for (j <- i to limit by i) {
          phi.update(j, phi(j) / i * (i - 1))
        }
      }
      result += phi(i)
    }
    result
  }

  println(run)
}