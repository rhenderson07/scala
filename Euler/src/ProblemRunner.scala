

object ProblemRunner extends App {
  val p = new Problem3()
  
  println("Problem %d: %s".format(p.number, p.description))
  println(p.run)
}