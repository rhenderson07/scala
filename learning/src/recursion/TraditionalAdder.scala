package recursion
import scala.annotation.tailrec

object TraditionalAdder extends App {
  
  // Sum elements of list with traditional recursion
  def sumList(intList: List[Int]): Int = {
    if (intList.isEmpty) 
      return 0 
    else 
      return intList.head + sumList(intList.tail) 
  }
  
  // Sum all integer values between 0 and x
  def sum(x: Int): Int = {
    if (x <= 0)
      return 0
    else
      return x + sum(x-1)
  }
  
  //val intList = List.range(0, 1000)
  //println("traditional recursion: " + sumList(intList))
  println("traditional recursion: " + sum(500000))
  
}