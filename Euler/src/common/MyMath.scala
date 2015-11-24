package common

object MyMath {
  val primes: Stream[Long] = 2L #:: Stream.from(3).map(_.longValue).filter(!divisibleByPrime(_))
  def divisibleByPrime(n: Long): Boolean = primes.takeWhile(i => i * i <= n).exists(n % _ == 0)
  def primeAt(n: Int): Long = primes.take(n).last

  def factorial(n: Int): BigInt = (1 to n).map(BigInt(_)).reduce(_ * _)

  // does the same as the above factorial function, with a more concise syntax.
  private def factorial2(n: Int): BigInt = (BigInt(1) /: (1 to n))(_ * _)

  /**
   * Sum the individual digits of the provided number
   */
  def asListOfDigits(n: BigInt): List[Int] = n.toString.toList.map(_.asDigit)

  // integer square root. does not have rounding errors
  def intSqrt(n: Int): Int = Stream.from(0).takeWhile(x => x * x <= n).last
  
  // return the next sq
  def intSqrtRoundUp(n: Int): Int = Stream.from(0).find(x => x * x >= n).get
}