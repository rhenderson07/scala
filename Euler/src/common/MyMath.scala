package common

object MyMath {
  lazy val primes: Stream[Long] = 2L #:: Stream.from(3).map(x => x.longValue).filter(i => primes.takeWhile(j => j * j <= i).forall(k => i % k != 0))
  
  def primeAt(n: Int): Long = primes.take(n).last
}