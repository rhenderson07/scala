package common.upgrades

import scala.annotation.tailrec
import scala.collection.LinearSeq

class MyLinearSeq[T](target: LinearSeq[T]) {

  /**
   * Return true if property applies to all items in sequence. Comparison may be non-reflexive (directional).
   */
  def holdsForFamily(property: (T, T) => Boolean): Boolean = {
    @tailrec
    def rec(candList: LinearSeq[T]): Boolean = {
      if (candList.length <= 1)
        true
      else if (candList.tail.forall(property(candList.head, _)))
        rec(candList.tail)
      else
        false
    }

    rec(target)
  }

  /**
   * This approach requires that the property be reflexive.
   */
  def holdsForFamilyReflexive[T <: Any](property: (T, T) => Boolean)(candList: LinearSeq[T]): Boolean = {
    candList.forall(x => candList.filter(_ != x).forall(y => property(x, y)))
  }

  /**
   * Returns an iterator/stream of all subsets with the defined length
   */
  def subSets[T <: Any](length: Int): LinearSeq[LinearSeq[T]] = {
    // TODO implement
    LinearSeq()
  }

  /**
   * Search a sorted sequence for an element. Return false once the elements expected position has been passed.
   * This operation assumes the sequence is already sorted. Can be performed on an infinite sequence, if the
   * sequence has elements above the element being search for.
   */
  def containsMonotonic[T <: Long](l: LinearSeq[T])(n: T): Boolean = {
    // TODO implement
    false
  }

  /**
   * Traverse a stream to find the first subset with a property
   */
  def findSet[T <: Any](property: LinearSeq[T] => Boolean)(size: Int): LinearSeq[T] = {
    // TODO implement
    LinearSeq()
  }
  
    /**
   * Return a list of frequencies for each element in the list. Will not return for infinite sequences.
   */
  def frequencies = {
    val totalCount = target.size
    target.groupBy(identity).mapValues(_.size * BigDecimal(100) / totalCount).toSeq
  }
}