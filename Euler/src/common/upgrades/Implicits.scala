package common.upgrades

import scala.collection.LinearSeq

/**
 * This is the "Pimp My Library pattern," defined by Odersky.
 */
object Implicits {
  implicit def stringToMyString[T](s: String) = new MyString(s)
  implicit def linearSeqToMyLinearSeq[T](l: LinearSeq[T]) = new MyLinearSeq(l)
}