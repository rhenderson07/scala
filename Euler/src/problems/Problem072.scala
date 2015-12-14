
package problems

import common.MyMath
import scala.collection.LinearSeq

object Problem072 extends Problem with App {
  def number = 72
  def description = "How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?"

  lazy val run = properFractionCountUnder3(target).longValue()
  //  lazy val run = fractionsLessThan(target).size.longValue()

  lazy val target = 3000

  // First attempt. very slow. 1.6 seconds for target 3000
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

  // second attempt. 0.9 seconds for target 3000
  def properFractionCountUnder(n: Int) = {
    (1 to n).map(properFactionCount).sum
  }
  def properFactionCount(n: Int) = {
    (1 to n).count(BigInt(n).gcd(_).equals(1))
  }

  // third attempt. Cache factors. slow. 1.8 seconds for target 3000
  private lazy val factorCache = scala.collection.mutable.Map[Int, LinearSeq[Long]]()

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

  // attempt to produce primes for all values between 1 and target
  def primeFactorMapTo(n: Int) = {
    for {
      x <- 1 to n
    } yield {
      MyMath.primeDivisors(x)
    }
  }

  // Solution from MathBlog:
  {
    val limit = 1000000
    val phi = Array.tabulate(limit)(x => x)
    lazy val result = 0

    val myResult = {
      for {
        i <- 2 to limit;
        j <- (i to limit) by i
        if (phi(i) == i)
          //phi(j) = 9
      } yield {
        i
      }
    }
  }

  //println(primeFactorMapTo(target).last.last)

  //println(run)
}