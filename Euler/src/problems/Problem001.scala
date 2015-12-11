package problems

object Problem001 extends Problem with App {
  def number = 1
  def description = "Find the sum of all the multiples of 3 or 5 below 1000."

  def run = multipleSum
  
  //First implementation
  lazy val multipleSum = (1 until 1000).par.filter((x: Int) => x % 3 == 0 || x % 5 == 0).reduce(_ + _)
  
  // second implementation. slightly cleaner syntax  
  lazy val multipleSum2 = (1 until 1000).par.filter(x => x % 3 == 0 || x % 5 == 0).sum
  
  println(run)
}