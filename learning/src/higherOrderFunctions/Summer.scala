package higherOrderFunctions

class Summer {

  // functions to pass
  def square(x: Int): Int = x * x
  def cube(x: Int): Int = x * x * x

  // use syntax sugar in Scala to declare a funcion 
  // which can take parameters of just f, or f a and b
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }

  sum(square)(1, 10)
  sum(cube)(1, 10)

}