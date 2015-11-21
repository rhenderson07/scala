

class Problem2 extends Problem with App{
  def number = 2
  def description = "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."

  def run = sumEvenFibs

  val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)
  
  val maxFib = 4000000
  def sumEvenFibs = fibs.takeWhile(_ <= maxFib).filter(_ % 2 == 0).sum
  
  
  fibs.takeWhile(_ < 4000000).foreach(println)
}