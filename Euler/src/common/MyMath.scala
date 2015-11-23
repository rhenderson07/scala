package common

object MyMath {
  lazy val primes: Stream[Long] = 2L #:: Stream.from(3).map(x => x.longValue).filter(i => primes.takeWhile(j => j * j <= i).forall(k => i % k != 0))
  
  def primeAt(n: Int): Long = primes.take(n).last
    
  
  def factorial(n:Int): BigInt = (1 to n).map(BigInt(_)).reduce(_*_)
  
  // does the same as the above factorial function, with a more concise syntax.
  private def factorial2(n:Int): BigInt = (BigInt(1) /: (1 to n)) (_*_)
  
  /**
   * Sum the individual digits of the provided number
   */
  def asListOfDigits(n :BigInt): List[Int] = n.toString.toList.map(_.asDigit)

}