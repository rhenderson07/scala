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

  // second attempt to find factor Count
  def factorCount2(n: Long, currentDivisorCount: Int = 0): Int = {
    if (n % 2 == 0)
      factorCount2(n / 2, currentDivisorCount + 1)
    else
      0

    //TODO fix according to http://code.jasonbhill.com/sage/project-euler-problem-12/
    //    val first = if (n % 2 == 0) n/2 else n
    //    
    //    val divisors = 1
    //    val count = 0
    //    
    //    while n % 2 == 0:
    //        count += 1
    //        n = n/2
    //    divisors = divisors * (count + 1)
    //    p = 3
    //    while n != 1:
    //        count = 0
    //        while n % p == 0:
    //            count += 1
    //            n = n/p
    //        divisors = divisors * (count + 1)
    //        p += 2
    //    return divisors
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