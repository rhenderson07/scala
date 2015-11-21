package problems

import scala.annotation.tailrec
import scala.BigInt
import scala.collection.immutable.Stream.consWrapper
import scala.math.BigInt.int2bigInt
import scala.math.BigInt.long2bigInt
import problems.Problem

object Problem3 extends Problem with App {
  def number = 3
  def description = "What is the largest prime factor of the number 600851475143."

  def run = largestPrimeFactor2(target)
  //def run = largestPrimeFactor(target)

  lazy val target = 600851475143L
  def root(value: Long): Long = Math.sqrt(value).round

  // my first attempt with streams. Only works with integer values
  lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i => primes.takeWhile(j => j * j <= i).forall(k => i % k != 0))
  def largestPrimeFactor2(value: Long) = primes.takeWhile(_ <= root(value)).filter(value % _ == 0).lastOption.getOrElse(-1)

  // second attempt with Streams. attempting with BigInt. very slow
  val BigIntStream = Stream.iterate(BigInt(3))({ (x: BigInt) => x + 1 }: BigInt => BigInt)
  lazy val bigIntPrimes: Stream[BigInt] = BigInt(2) #:: BigIntStream.filter(i => bigIntPrimes.takeWhile(j => j * j <= i).forall(k => i % k != 0))
  def root(value: BigInt) = BigIntStream.find(_.pow(2) > value).getOrElse(BigInt(-1))
  def largestPrimeFactor3(value: BigInt) = bigIntPrimes.takeWhile(_ <= root(value)).filter(value % _ == 0).lastOption.getOrElse(-1)

  //from stack. Does not return
  def primesUnder(n: Long): List[Long] = {
    require(n >= 2)

    @tailrec
    def rec(i: Long, primes: List[Long]): List[Long] = {
      if (i >= n) primes
      else if (divisorInList(i, primes)) rec(i + 1, i :: primes)
      else rec(i + 1, primes)
    }

    rec(2, List()).reverse
  }
  def divisorInList(num: Long, factors: List[Long]): Boolean = factors.forall(num % _ != 0)
  def largestPrimeFactor(value: Long) = primesUnder(root(value)).filter(value % _ == 0).lastOption.getOrElse(-1L)


  //p.primes.takeWhile(_ <= 1000).foreach(println)
  println(largestPrimeFactor3(100000L))
}