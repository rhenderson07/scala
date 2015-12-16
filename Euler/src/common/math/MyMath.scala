package common.math

import scala.math.BigDecimal.RoundingMode
import scala.annotation.tailrec
import scala.BigDecimal
import scala.BigInt
import scala.collection.immutable.Stream.consWrapper
import scala.math.BigDecimal.int2bigDecimal
import scala.math.BigInt.int2bigInt

object MyMath {
  val primes: Stream[Long] = 2L #:: Stream.from(3, 2).map(_.toLong).filter(!divisibleByAnyPrime(_))
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
  def findDivisors(n: Long): List[Long] = {
    @tailrec
    def rec(x: Long, remainingPrimes: Stream[Long], currentFactors: List[Long]): List[Long] = {
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

    rec(n, primes, List(1L)) //.sorted
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
  def sumOfDigits(n: BigInt): Long = asListOfDigits(n).map(_.toLong).sum

  val squares: Stream[Long] = Stream.from(0).map(x => x.toLong * x)

  /**
   *  Return the last or current integer square root. Avoids the rounding errors of floating point math.
   */
  def intSqrt(n: Int): Int = squares.indexWhere(_ > n) - 1

  /**
   *  Return the next or current sqrt.
   */
  def intSqrtRoundUp(n: Int): Int = squares.indexWhere(_ >= n)

  /**
   * Square root using BigDecimal. Very accurate. Base precision is 34
   */
  def sqrt(A: BigDecimal): BigDecimal = {
    val TWO = BigDecimal(2)

    def rec(lowBid: BigDecimal, highBid: BigDecimal): BigDecimal = {
      if (lowBid.equals(highBid))
        highBid
      else {
        val newHigh = (highBid + A / highBid) / TWO
        rec(highBid, newHigh)
      }
    }

    //TODO remove setScale if this does not improve accuracy
    val initialLow = BigDecimal(0)
    val initialHigh = BigDecimal(Math.sqrt(A.doubleValue())).setScale(100)
    rec(initialLow, initialHigh)
  }

  /**
   * Private utility method used to compute the square root of a BigDecimal.
   *
   * Defined at http://stackoverflow.com/questions/13649703/
   *
   * @author Luciano Culacciatti
   * @url http://www.codeproject.com/Tips/257031/Implementing-SqrtRoot-in-BigDecimal
   */
  def bigSqrt(scale: Int)(value: BigDecimal): BigDecimal = {
    val calcuationContext = new java.math.MathContext(scale + 10)
    val ONE = BigDecimal(1).apply(calcuationContext)
    val SQRT_DIG = BigDecimal(scale, calcuationContext)
    val SQRT_PREC = BigDecimal(10, calcuationContext).pow(SQRT_DIG.intValue())

    val precision = ONE / SQRT_PREC
    val n = value.apply(calcuationContext)

    @tailrec
    def sqrtNewtonRaphson(xn: BigDecimal): BigDecimal = {
      val fx = xn.pow(2) - n
      val fpx = xn * 2
      val xn1 = xn - (fx / fpx.setScale(2 * precision.intValue(), RoundingMode.HALF_DOWN))
      val currentSquare = xn1.pow(2);
      val currentPrecision = (currentSquare - n).abs

      if (currentPrecision.compare(precision) < 0) {
        val returnValContext = new java.math.MathContext(scale)
        xn1.apply(returnValContext)
      } else
        sqrtNewtonRaphson(xn1)
    }

    sqrtNewtonRaphson(ONE);
  }
}