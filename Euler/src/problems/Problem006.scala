package problems

object Problem006 extends Problem with App {
  def number = 6
  def description = "Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."

  def run = sumSquareDiff(100)

  def square(x: Long): Long = x * x

  def sum(f: Long => Long)(a: Long, b: Long): Long = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }

  def sumSquareDiff(maxVal: Int): Long = square(sum(x => x)(1, maxVal)) - sum(square)(1, maxVal)
  
  println(sumSquareDiff(100))
}