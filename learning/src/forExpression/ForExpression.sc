package forExpression

object ForExpression {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val s = for (x <- 1 to 5 if x * x > 10) yield 2 * x
                                                  //> s  : scala.collection.immutable.IndexedSeq[Int] = Vector(8, 10)

}