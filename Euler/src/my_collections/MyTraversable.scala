package my_collections

import scala.annotation.tailrec

class MyTraversable[T](target: Traversable[T]) {
  implicit def listToMyTraversable[T](l: Traversable[T]) = new MyTraversable(l)

  // traverse a stream to find the first subset with a property
  def findSet(property: List[T] => Boolean)(size: Int): List[T] = {
    List()
  }

  @tailrec
  final def holdsForFamily(property: (T, T) => Boolean)(candList: Traversable[T] = target): Boolean = {
    if (candList.isEmpty)
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