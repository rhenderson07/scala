package common

import scala.annotation.tailrec

object Lists {
  @tailrec
  def holdsForFamily3[T <: Any](property: (T, T) => Boolean)(candList: List[T]): Boolean = {
    if (candList.length <= 1)
      true
    else if (candList.tail.forall(property(_, candList.head)))
      holdsForFamily3(property)(candList.tail)
    else
      false
  }

  def holdsForFamily[T <: Any](property: (T, T) => Boolean)(candList: Stream[T]): Boolean = {
    candList.forall(x => candList.filter(_ != x).forall(y => property(x, y)))
  }

  /**
   * returns an iterator/stream of all subsets with the defined length
   */
//  def subSets[T <: Any](length: Int): List[List[T]] = {
//    
//  }

  // TODO define this in a common list function
  def containsMonotonic[T <: Long](l: List[T])(n: T): Boolean = {
    l.takeWhile(_ <= n).contains(n)
  }
}