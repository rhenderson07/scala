package patternMatching

object PaternMatcher extends App{
  def price(product:String):Double = {
    product match {
      case "apple" => 1.29
      case "banana" => .89
      case _ => 0.00
    }
  }
  
  val products = List("apple", "banana", "pair")  
  products.foreach(p=>println(p + ": $" + price(p)))
}