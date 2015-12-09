package common

import scala.collection.mutable.LongMap
import scala.annotation.tailrec

object MyMath {
  val primes: Stream[Long] = 2L #:: Stream.from(3, 2).map(_.longValue).filter(!divisibleByAnyPrime(_))
  def divisibleByAnyPrime(n: Long): Boolean = primes.takeWhile(i => i * i <= n).exists(n % _ == 0)

  def primeDivisors(n: Long) = primes.takeWhile(_ <= n).filter(n % _ == 0)

  /**
   * First attempt at finding divisors.
   * Very slow.
   * Checks every number between 1 and n.
   */
  private def findDivisorsFail1(n: Long) = {
    for (
      x <- 1L to n if (n % x == 0)
    ) yield x
  }

  /**
   * Second attempt at finding divisors
   * Use prime divisors, to perform operation extremely quickly.
   * Return all divisors of the value n.
   */
  def findDivisors(n: Long) = {
    @tailrec
    def rec(x: Long, remainingPrimes: Stream[Long], currentFactors: List[Long] = List(1L)): List[Long] = {
      val cand = remainingPrimes.head

      if (x == 1) {
        currentFactors
      } else if (x % cand == 0) {
        // find powers of cand that divide n.
        val powersOfCand = Stream.from(1).map(power(cand, _)).takeWhile(x % _ == 0)

        // multiply each power by previously identified factors
        val newDivisors = powersOfCand.map(p => currentFactors.map(x => x * p)).flatten.toList

        rec(x / powersOfCand.max, remainingPrimes.tail, newDivisors ::: currentFactors)
      } else {
        rec(x, remainingPrimes.tail, currentFactors)
      }
    }

    rec(n, primes).sorted
  }

  /**
   * First attempt at counting factors, using standard recursion. Performs very slow
   */
  private def divisorCountFail1(n: Long): Int = {
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

  /**
   * Second attempt to find factor Count. Factors out powers of odd values. Does not focus exclusively on primes.
   * This solution follows an imperative paradigm.
   * Process described at http://code.jasonbhill.com/sage/project-euler-problem-12/
   */
  private def divisorCountFail2(n: Long): Int = {
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

  /**
   *  Third attempt to find factor count.
   *  Factors out all primes, using tail recursion.
   *  Runs extremely fast
   */
  def divisorCount(n: Long): Int = {
    findDivisors(n).length
  }

  /**
   * Power operation that only operates on Long values.
   * This is more accurate than Math.pow, which returns a double.
   * This is faster than BigInt.Pow() which performs costly conversions.
   */
  def power(n: Long, pow: Int): Long = {
    @tailrec
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

  /**
   * Perform the factorial function:
   * n! = 1 * 2 * ... * (n-1) * n
   */
  def factorial(n: Int): BigInt = (1 to n).map(BigInt(_)).reduce(_ * _)

  /**
   * Does the same as the above factorial function, with a more concise, foldLeft syntax.
   */
  private def factorial2(n: Int): BigInt = (BigInt(1) /: (1 to n))(_ * _)

  /**
   * Return the individual digits of the provided number
   */
  def asListOfDigits(n: BigInt): List[Int] = n.toString.toList.map(_.asDigit)
  def asListOfDigits(n: Int): List[Int] = n.toString.toList.map(_.asDigit)

  /**
   * sum all the digits in a BigInt. This is a common operation in Project Euler
   */
  def sumOfDigits(n: BigInt): Long = asListOfDigits(n).map(_.longValue).sum

  val squares: Stream[Long] = Stream.from(0).map(x => x.longValue * x)

  /**
   *  Return the last or current integer square root. Avoids the rounding errors of floating point math.
   */
  def intSqrt(n: Int): Int = squares.indexWhere(_ > n) - 1

  /**
   *  Return the next or current sqrt.
   */
  def intSqrtRoundUp(n: Int): Int = squares.indexWhere(_ >= n)
}