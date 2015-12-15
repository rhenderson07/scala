
package problems

trait Problem {
  def number: Int
  def name: String = "Problem " + number
  def description: String
  def run: Long

  def time[A <: Any, B <: Any](f: (A => B))(param: A): Unit = {
    println("Problem %d: %s".format(number, description))
    
    val startTime = System.nanoTime();
    val solution = f(param)
    val endTime = System.nanoTime();
    println(s" Solution = $solution.  Elapsed time ${(endTime - startTime) * 1e-9} seconds")
  }
}