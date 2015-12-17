package common.upgrades

/**
 * This is the "Pimp My Library pattern," defined by Odersky.
 */
object Implicits {
  implicit def stringToMyString[T](s: String) = new MyString(s)
  implicit def listToMyList[T](l: List[T]) = new MyLinearSeq(l)
}