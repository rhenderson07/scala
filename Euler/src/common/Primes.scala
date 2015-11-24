package common

object Primes {

  val primes: Stream[Long] = 2L #:: Stream.from(3).map(_.longValue).filter(!divisibleByPrime(_))

  def divisibleByPrime(n: Long): Boolean = primes.takeWhile(i => i * i <= n).exists(n % _ == 0)

  def primeAt(n: Int): Long = primes.take(n).last
}