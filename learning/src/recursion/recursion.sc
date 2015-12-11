package recursion
import scala.annotation.tailrec

object recursion extends App{
  val intList = List.range(1, 1001)

  //Sum elements of list with tail recursion
  @tailrec
  def tailRecurseSum(myList: List[Int], currentSum: Int = 0): Int = {
    if (myList.isEmpty)
      return currentSum
    else
      return tailRecurseSum(myList.tail, currentSum + myList.head)
  }

  println(sum(List.range(1, 1001)))
  println(tailRecurseSum(List.range(1, 1001)))
  
  
	
}