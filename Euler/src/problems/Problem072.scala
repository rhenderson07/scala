
package problems

import common.math.MyMath
import scala.collection.LinearSeq
import scala.collection.mutable.HashTable
import scala.collection.parallel.mutable.ParHashMap
import scala.annotation.tailrec

object Problem072 extends Problem with App {
  def number = 72
  def description = "How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?"

  lazy val run = success1(target).longValue()
  //  lazy val run = fractionsLessThan(target).size.longValue()

  lazy val target = 1000000

  // First attempt. very slow. 1.6 seconds for target 3000
  def fail1(n: Int) = {
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

    fractionsLessThan(n).size
  }

  // second attempt. 0.9 seconds for target 3000
  def fail2(n: Int) = {
    def properFractionCountUnder(n: Int) = {
      (1 to n).map(properFactionCount).sum
    }
    def properFactionCount(n: Int) = {
      (1 to n).count(BigInt(n).gcd(_).equals(1))
    }

    properFractionCountUnder(n)
  }

  // third attempt. Cache factors. slow. 1.8 seconds for target 3000
  def fail3(n: Int) = {
    lazy val factorCache = scala.collection.mutable.Map[Int, LinearSeq[Long]]()

    def factors(number: Int) = {
      factorCache.getOrElseUpdate(number, MyMath.primeDivisors(number))
    }

    def properFractionCountUnder(n: Int) = {
      (1 to n).map(properFactionCount).sum
    }

    def properFactionCount(n: Int) = {
      (1 to n).count(!shareFactor(_, n))
    }

    def shareFactor(a: Int, b: Int) = {
      factors(a).intersect(factors(b)).nonEmpty
    }

    properFractionCountUnder(n)
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
  def success1(n: Int) = {
    def properFractionCountUnder(limit: Int): Long = {
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

    properFractionCountUnder(n).longValue()
  }

  // Fifth attempt, counting terms in the Farey series. Very slow, but eventually terminates
  def fail4(n: Int) = {

    def fareyTerms(n: Int) = {
      @tailrec
      def rec(a: Int, b: Int, c: Int, d: Int, currentCount: Long): Long = {
        if (c > n) {
          currentCount - 1 // remove 1 to avoid counting the value 0/1
        } else {
          val k = (n + b) / d
          rec(c, d, (k * c - a), (k * d - b), currentCount + 1)
        }
      }

      rec(0, 1, 1, n, 0)
    }

    fareyTerms(n)
  }

  def fail5(n: Int) = {
    // represent function as a val
    val nextFareyTermVal = (a: Int, b: Int, c: Int, d: Int) => {
      val k = (n + b) / d
      (c, d, (k * c - a), (k * d - b))
    }

    // represent function as a def
    def nextFareyTerm(a: Int, b: Int, c: Int, d: Int) = {
      val k = (n + b) / d
      (c, d, (k * c - a), (k * d - b))
    }

    //iterator for farey terms
    val fareyGenerator = Iterator.iterate((0, 1, 1, n))((nextFareyTermVal).tupled)

    def properFractionCountUnder(n: Int) = fareyGenerator.takeWhile(_._3 <= n).size - 1

    properFractionCountUnder(n)
  }

  time(success1)(target)
}