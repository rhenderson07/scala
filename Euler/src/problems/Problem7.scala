package problems

object Problem7 extends Problem with App {
  def number = 7
  def description = "What is the 10 001st prime number?"

  def run = primeAt(10001)

  // same function defined in common.Primes.primeAt
  lazy val primes: Stream[Long] = 2L #:: Stream.from(3).map(x => x.longValue).filter(i => primes.takeWhile(j => j * j <= i).forall(k => i % k != 0))
  def primeAt(n: Int): Long = primes.take(n).last
}