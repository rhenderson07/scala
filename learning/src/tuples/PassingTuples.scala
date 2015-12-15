package tuples

object PassingTuples {
  //There is a special tupled method available for every function:

  // able to tuple a function when it is represented as a val
  def foo(): (Int, Int) = (1, 1)
  val bar = (a: Int, b: Int) => (a, b)
  val bar2 = (bar).tupled

  // Able to pass the partially evaluated function
  def myFunct(a: Int, b: Int) = (a, b)
  val myVal = (myFunct _).tupled

  // alternate syntax
  val baz = Function.tupled(myFunct _)
}