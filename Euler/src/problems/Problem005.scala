package problems

import scala.annotation.tailrec

object Problem5 extends Problem with App {
  def number = 5
  def description = "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"

  def run = smallestDivisible2

  // third attempt. this worked, fast

  def findVeryDivisible(maxDivisor: Int): Long = {
    @tailrec
    def rec(candidate: Long, divisor: Int): Long = {
      if (divisor <= 0)
        candidate
      else if (candidate % divisor == 0)
        rec(candidate, divisor - 1)
      else
        rec(candidate + 1, maxDivisor)
    }
    rec(1, maxDivisor)
  }
  lazy val smallestDivisible2 = findVeryDivisible(20)
  //println(findVeryDivisible(20))

  // first attempt. runs out of memory
  {
    def divisible = (x: Int) => (1 to 20).toList.forall(x % _ == 0)
    lazy val longStream = Stream.iterate(1L)({ (x: Long) => x + 1 }: Long => Long)
    lazy val smallestDivisible = longStream.find((x: Long) => (1 to 20).toList.forall((i: Int) => x % i == 0)).get
    print("")
  }

  // second attempt. multiply all values together to get an upper bound, then count down. did not need this approach
  {
    def product(nums: List[Int]): Long = {
      if (nums.isEmpty)
        1
      else
        nums.head * product(nums.tail)
    }
    lazy val upperBound = product((1 to 20).toList)
    print("")
  }

  // from euler. faster than mine, by alot
  {
    def greatestCommonDivisor(m: Long, n: Long): Long = if (m == 0) n else greatestCommonDivisor(n % m, m)
    def leastCommonMultiple(m: Long, n: Long): Long = m * n / greatestCommonDivisor(m, n)
    lazy val A = Stream.from(1).map(x => x.longValue).take(20).foldLeft(1L)((x: Long, y: Long) => leastCommonMultiple(x, y))

    println(A)
  }

}