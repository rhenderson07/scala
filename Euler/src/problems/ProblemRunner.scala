package problems

object ProblemRunner extends App {

  val p = Problem014

  println("Problem %d: %s".format(p.number, p.description))

  val startTime = System.nanoTime();
  val solution = p.run
  val endTime = System.nanoTime();
  println(s" Solution = $solution.  Elapsed time ${(endTime - startTime) * 1e-9} seconds")
}