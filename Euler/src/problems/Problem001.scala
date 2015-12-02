package problems

object Problem001 extends App {
  def number = 1
  def description = "Find the sum of all the multiples of 3 or 5 below 1000."

  def run = (0 until 1000).par.filter((x: Int) => x % 3 == 0 || x % 5 == 0).reduce(_ + _)
  
  println(run)
}