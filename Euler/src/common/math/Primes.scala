package common.math

object Primes {
  val stream: Stream[Long] = 2L #:: Stream.from(3, 2).map(_.toLong).filter(!divisibleByAnyPrime(_))
  def divisibleByAnyPrime(n: Long): Boolean = stream.takeWhile(i => i * i <= n).exists(n % _ == 0)
}