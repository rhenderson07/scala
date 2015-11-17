package squareRoot

object squareRootScratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0000001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  val square = 2.4e50                             //> square  : Double = 2.4E50
  val root = sqrt(square)                         //> root  : Double = 1.5491933450166833E25
  root * root                                     //> res0: Double = 2.4000000202439806E50
}