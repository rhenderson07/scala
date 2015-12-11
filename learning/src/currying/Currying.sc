package currying

object Currying {

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  def factorial(x: Int): Int = product(x => x)(1, x)
                                                  //> factorial: (x: Int)Int

  product(x => x * x)(3, 4)                       //> res0: Int = 144
  factorial(5)                                    //> res1: Int = 120
  
  
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }                                               //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
 
  def productMapReduce(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)          //> productMapReduce: (f: Int => Int)(a: Int, b: Int)Int
  
}