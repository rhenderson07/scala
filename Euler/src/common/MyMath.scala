package common

import scala.collection.mutable.LongMap
import scala.annotation.tailrec

object MyMath {
  val primes: Stream[Long] = 2L #:: Stream.from(3).map(_.longValue).filter(!divisibleByAnyPrime(_))
  def divisibleByAnyPrime(n: Long): Boolean = primes.takeWhile(i => i * i <= n).exists(n % _ == 0)

  def primeDivisors(n: Long) = primes.takeWhile(_ <= n).filter(n % _ == 0)

  // First attempt at counting factors, using standard recursion. Slow
  def factorCount(n: Long): Int = {
    // recursively find combinations that work
    def rec(value: Long, factorList: List[Long]): Int = {
      if (value == 1L)
        1
      else if (factorList.isEmpty || value < 1L)
        0
      else if (value % factorList.head != 0)
        rec(value, factorList.tail)
      else
        1 + rec(value / factorList.head, factorList) + rec(value, factorList.tail)
    }

    val primeFacts = primeDivisors(n).toList
    rec(n, primeFacts)
  }

  // second attempt to find factor Count. This is not functional at all
  // Process describe at http://code.jasonbhill.com/sage/project-euler-problem-12/
  def factorCount2(n: Long): Int = {
    var value = n

    // divide out 2 as many times as possible
    var count = 0
    while (value % 2 == 0) {
      count += 1
      value = value / 2
    }
    var divisors = count + 1

    // divide out odd numbers as many times as possible
    var p = 3
    while (value > 1) {
      var count2 = 0

      while (value % p == 0) {
        count2 += 1
        value = value / p
      }

      divisors = divisors * (count2 + 1)
      p += 2
    }

    return divisors
  }

  // third attempt to find factor count. factor out all primes, using tail recursion
  def factorCount3(n: Long): Int = {
    @tailrec
    def rec(x: Long, remainingPrimes: Stream[Long], currentFactorCount: Int = 1): Int = {
      val cand = remainingPrimes.head

      if (x == 1) {
        currentFactorCount
      } else if (x % cand == 0) {
        // find max power of cand that divides n. Returns stream of tuple with form (power, cand * power) 
        val powersOfCand = Stream.from(1).map(p => (p, integerPower(cand, p)))
        val maxDivisor = powersOfCand.takeWhile(x % _._2 == 0).last

        rec(x / maxDivisor._2, remainingPrimes.tail, currentFactorCount * (maxDivisor._1 + 1))
      } else {
        rec(x, remainingPrimes.tail, currentFactorCount)
      }
    }

    rec(n, primes)
  }

  def integerPower(n: Long, pow: Int): Long = {
    def rec(value: Long, timesToApply: Int, currentTotal: Long = 1): Long = {
      if (timesToApply == 0)
        1L
      else if (timesToApply == 1)
        currentTotal
      else
        rec(value, timesToApply - 1, currentTotal * value)
    }
    rec(n, pow, n)
  }

  def factorial(n: Int): BigInt = (1 to n).map(BigInt(_)).reduce(_ * _)

  // does the same as the above factorial function, with a more concise syntax.
  private def factorial2(n: Int): BigInt = (BigInt(1) /: (1 to n))(_ * _)

  /**
   * Return the individual digits of the provided number
   */
  def asListOfDigits(n: BigInt): List[Int] = n.toString.toList.map(_.asDigit)

  /**
   * sum all the digits in a BigInt. This is a common operation in Project Euler
   */
  def sumOfDigits(n: BigInt): Long = asListOfDigits(n).map(_.longValue).sum

  val squares: Stream[Long] = Stream.from(0).map(x => x.longValue * x)

  // return the last or current integer square root. Avoids the rounding errors of floating point math
  def intSqrt(n: Int): Int = squares.indexWhere(_ > n) - 1

  // return the next or current sqrt.
  def intSqrtRoundUp(n: Int): Int = squares.indexWhere(_ >= n)

  // find divisors
  def findDivisors(n: Long) = {
    for (
      x <- 1L to n if (n % x == 0)
    ) yield x
  }
}