package higherOrderFunctions

object Adder {
  // form of higher order function
  def highFunc(lowFunc: Int => String, a: Int): String = lowFunc(a)
                                                  //> highFunc: (lowFunc: Int => String, a: Int)String

  // define higher order function
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int

  // define functions to pass
  def id(x: Int): Int = x                         //> id: (x: Int)Int
  def cube(x: Int): Int = x * x * x               //> cube: (x: Int)Int
  def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)
                                                  //> fact: (x: Int)Int

  // use result of higher order function
  def sumInts(a: Int, b: Int) = sum((x: Int) => x, a, b)
                                                  //> sumInts: (a: Int, b: Int)Int
  def sumCubes(a: Int, b: Int) = sum((x: Int) => x * x * x, a, b)
                                                  //> sumCubes: (a: Int, b: Int)Int
  def sumFactorials(a: Int, b: Int) = sum(fact, a, b)
                                                  //> sumFactorials: (a: Int, b: Int)Int

  // use currying to remove repetition of passing all parameters for each type of sum
  // sumMostlyCurried is a function that returns a sum function.
  def sumMostlyCurried(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  }                                               //> sumMostlyCurried: (f: Int => Int)(Int, Int) => Int

  // able to pass cube function as parameter, and use resulting function to evaluate params (1, 10)
  sumMostlyCurried(cube)(1, 10)                   //> res0: Int = 3025

  // use syntax sugar in Scala to declare a funcion which can take parameters of just f, or f a and b
  def sumFullyCurried(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumFullyCurried(f)(a + 1, b)
  }                                               //> sumFullyCurried: (f: Int => Int)(a: Int, b: Int)Int
  
  def sumCubes2 = sumFullyCurried(cube)_ // underscore marks result as partially evaluated function
                                                  //> sumCubes2: => (Int, Int) => Int
  sumFullyCurried(cube)(1, 15)                    //> res1: Int = 14400


}