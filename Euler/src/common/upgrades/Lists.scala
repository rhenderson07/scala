package common.upgrades

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq

object Lists {
  @tailrec
  def holdsForFamily[T <: Any](property: (T, T) => Boolean)(candList: LinearSeq[T]): Boolean = {
    if (candList.length <= 1)
      true
    else if (candList.tail.forall(property(candList.head, _)))
      holdsForFamily(property)(candList.tail)
    else
      false
  }

  /**
   * This approach requires that the property be reflexive.
   */
  def holdsForFamilyReflexive[T <: Any](property: (T, T) => Boolean)(candList: LinearSeq[T]): Boolean = {
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

  // traverse a stream to find the first subset with a property
  def findSet[T <: Any](property: List[T] => Boolean)(size: Int): List[T] = {
    List()
  }
}