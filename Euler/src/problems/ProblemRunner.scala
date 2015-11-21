package problems

object ProblemRunner extends App {
  val p = Problem13
  
  println("Problem %d: %s".format(p.number, p.description))
  println(p.run)
}