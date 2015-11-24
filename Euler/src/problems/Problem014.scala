
package problems

import scala.annotation.tailrec

object Problem014 extends Problem with App {
  def number = 14
  def description = "Which starting number, under one million, produces the longest Collatz chain?"

  def run = maxColLen._1
  
  private final val upperBound = 1000000

  /* Apply collatz recurisvely:
   * n → n/2 (n is even)
   * n → 3n + 1 (n is odd)
   */
  @tailrec
  def collatzLen(n: Long, len: Int = 1): Long = {
    if (n == 1)
      len
    else if (n % 2 == 0)
      collatzLen(n / 2, len + 1)
    else
      collatzLen(n * 3 + 1, len + 1)
  }

  lazy val maxColLen = (1 to upperBound).map(x => (collatzLen(x), x)).max //.foreach(println)

  println(maxColLen)
}