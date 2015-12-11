package streams

class FunctionPassing {
    // use def to define function, pass function definition
  def increment(x: Int): Int = x + 1
  val myIntStream = Stream.iterate(0)(increment)

  // use val to create assign anonymous function as value, pass function value
  val addOne = (x: Int) => x + 1
  val myIntStream2 = Stream.iterate(0)(addOne: Int => Int)

  // pass anonymous function value
  val myIntStream3 = Stream.iterate(0)({ (x: Int) => x + 1 }: Int => Int)
}