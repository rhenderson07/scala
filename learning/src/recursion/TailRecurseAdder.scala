package recursion
import scala.annotation.tailrec

object TailRecurseAdder extends App {

  //Sum elements of list with tail recursion  
  @tailrec
  def sumList(intList: List[Int], currentSum: Int = 0): Int = {
    if (intList.isEmpty)
      return currentSum
    else
      return sumList(intList.tail, currentSum + intList.head)
  }

  // Sum all integer values between 0 and x, using tail recursion
  @tailrec
  def sum(x: Int, currentSum: Int = 0): Int = {
    if (x <= 0)
      return currentSum
    else
      return sum(x - 1, x + currentSum)
  }
  
  //val intList = List.range(0, 100000)
  //println("Tail recursion: " + sumList(intList))
  println("Tail recursion: " + sum(500000))
}