package problems

object Problem009 extends Problem with App {
  def number = 9
  def description = "There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc."

  def run = brute.last

  lazy val target = 1000

  def isTriplet(a: Int, b: Int, c: Int): Boolean = a * a + b * b == c * c

  def brute = {
    for (a <- 1 to target; b <- a to target; c <- b to target; if isTriplet(a, b, c) && a + b + c == 1000) yield a * b * c
  }

  println(run)
}