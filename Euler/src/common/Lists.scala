package common

import scala.annotation.tailrec

object Lists {
  @tailrec
  def holdsForFamily[T <: Any](property: (T, T) => Boolean)(candList: List[T]): Boolean = {
    if (candList.length <= 1)
      true
    else if (candList.tail.forall(property(_, candList.head)))
      holdsForFamily(property)(candList.tail)
    else
      false
  }

  // TODO define this in a common list function
  def containsMonotonic[T <: Long](l: List[T])(n: T): Boolean = {
    l.takeWhile(_ <= n).contains(n)
  }
}